module rec Transform

open ts2fable
open ts2fable.Syntax
open System
open System.Collections.Generic



// TODO: Interop between ts2fable and TypeProvider 
// The ts2fable package is adjusted to return .json files with the type information instead of the .fs file

type SimpleCache<'a>() =
     let dict = Dictionary<string, 'a>()
     let tryFind name =
        match dict.TryGetValue name with 
        | false, _ -> None
        | true, v -> Some v
     let addOrUpdate name addValue updater = 
        match dict.TryGetValue name with 
        | false, _ -> 
            dict.[name] <- addValue
            dict.[name]
        | true, v ->
            dict.[name] <- updater v
            dict.[name]

     member this.Get(name: string) =
        tryFind name

     member this.AddOrUpdate(name: string, value : 'a, updater) =
        addOrUpdate name value updater

let getFsTypeName = function 
    | FsType.Function f -> Option.defaultValue "fun" f.Name
    | FsType.Interface i -> i.Name
    | FsType.Variable v -> v.Name
    | FsType.ExportAssignment i -> "Export."+i
    | FsType.Import(FsImport.Type(t)) -> sprintf "%s.%s" t.SpecifiedModule t.ImportSpecifier.Name
    | FsType.Import(FsImport.Module(t)) -> t.Module
    | _ -> ""

let rec private findByName (types: Dictionary<string, FsType>) name = 
    match types.TryGetValue name with 
    | false, _ when name.Contains(".") -> 
        findByName types (name.Split('.') |> Array.tail |> String.concat " ")
    | false, _ -> 
        match types.Keys |> Seq.tryFind(fun x -> x.EndsWith name) with 
        | Some fb -> findByName types fb
        | None -> None
    | true, v -> Some v

type LibType =
     | ReactNode
     | ReactComponent

type ErasedType =
    | Any 
    | Bool 
    | Int 
    | Float
    | String 
    | Array of ErasedType
    | Option of ErasedType
    | Custom of  {| Name: string; Properties: (string * ErasedType) list;|}
    | ReactComponent of {| Name: string; Properties: ErasedType option|}
    | Fn of {| Name: string; Parameters: (string * ErasedType) list; ReturnType: ErasedType |}
    | Union of ErasedType list
    | Enum of FsEnum
    | LibType of LibType * ErasedType option

let createSimplifiedTypeMap (file: FsFileOut) =
    let rec collectFromModule (fsModule: FsModule) = seq {
                for f in fsModule.Types do
                    match f with 
                    | FsType.Module m -> yield! collectFromModule m
                    | t -> yield fsModule.Name, t
            }
       
    let types = file.Files 
                |> Seq.collect(fun m -> m.Modules |> Seq.collect collectFromModule)

    types |> Seq.map(fun(_,t) -> t) |> Seq.toList,
    types 
    |> Seq.fold(fun (acc: Dictionary<string, FsType>) (moduleName, type') -> 
        let typeName = (getFsTypeName type')
        match acc.TryGetValue (sprintf "%s.%s" moduleName typeName) with
        | false, _ -> 
            acc.[(sprintf "%s.%s" moduleName typeName)] <- type'
            acc
        | true, _ -> failwith "Duplicate Type in Module") (Dictionary<string, FsType>())


let getTypeSignature (typeMap: Dictionary<string, FsType>) (toResolve: FsType) =
    match toResolve with 
    | FsType.Mapped m ->
        match findByName typeMap m.Name with 
        | Some f -> 
            let signature = getTypeSignature typeMap f
            signature
        | _ -> match m.Name with 
               | "string" -> ErasedType.String
               | "int" -> ErasedType.Int
               | "float" -> ErasedType.Float
               | "bool" -> ErasedType.Bool
               | "any" -> ErasedType.Any
               | "React.ReactNode" -> ErasedType.LibType(LibType.ReactNode, None)
               | _ -> ErasedType.String
    
    | FsType.Interface i ->
        let inheritsFrom = i.Inherits |> List.map (getTypeSignature typeMap) |> List.tryHead // atm only single react component inheritance is supported
        match inheritsFrom with 
        | Some parent -> 
            match parent with 
            | LibType(LibType.ReactComponent, prop) -> 
                ErasedType.ReactComponent({| Name = i.Name; Properties = prop |})
            | _ -> ErasedType.String
        | None ->
            match i.Members with 
            | FsType.Function(f)::[] when f.Kind = FsFunctionKind.Constructor -> 
                let ret = getTypeSignature typeMap f.ReturnType
                ret
            | FsType.Function(f)::[] -> 
                getTypeSignature typeMap (FsType.Function(f))
            | _ ->
                let members = i.Members 
                              |> List.map (function 
                                           | FsType.Property p -> p.Name, 
                                                                    if p.Option then ErasedType.Option(getTypeSignature typeMap p.Type)
                                                                    else getTypeSignature typeMap p.Type
                                           | FsType.Function f -> (Option.defaultValue "invoke" f.Name), getTypeSignature typeMap (FsType.Function(f))
                                           | _ -> failwith "Interface Member not yet supported" )
                ErasedType.Custom({| Name = i.Name; Properties = members |})
    | FsType.Function f ->
        let returnType = getTypeSignature typeMap f.ReturnType
        let params' = f.Params |> List.map (fun p -> p.Name,      
                                                     if p.Optional then ErasedType.Option(getTypeSignature typeMap p.Type)
                                                     else getTypeSignature typeMap p.Type)
        ErasedType.Fn({| Name = Option.defaultValue "invoke" f.Name; Parameters = params'; ReturnType = returnType|})
    | FsType.Generic g ->
        match g.Type with 
        | FsType.Mapped m when m.Name = "Component" ->
             match g.TypeParameters with 
             | FsType.Mapped(props)::_ -> 
                 match findByName typeMap props.Name with 
                 | Some t ->  ErasedType.LibType(LibType.ReactComponent, Some(getTypeSignature typeMap t))
                 | _ -> failwith "could not find property type"
             | _ -> failwith "Non React Generics not supported yet"
        | t -> getTypeSignature typeMap t
        | _ -> failwith "Expected mapped type as generic type parameter"
    | FsType.Union u ->
        let possibleTypes = 
            u.Types
            |> List.map (fun t -> if u.Option then ErasedType.Option(getTypeSignature typeMap t) else getTypeSignature typeMap t)

        ErasedType.Union(possibleTypes)
    | FsType.Enum enum -> 
        ErasedType.Enum enum
    | _ -> failwith "oopsie"

let private find (types: FsType seq) (matcher: FsType -> 'a option) = 
    types |> Seq.choose matcher
 
type ApiExport = {
    Name: string
    Type': ErasedType
 }

let buildApi (typeMap: Dictionary<string, FsType>) =
    let exports = find typeMap.Values (function | FsType.Variable v -> Some v | _ -> None)
    exports |> Seq.map(fun export ->
                match export.Type with 
                | FsType.Mapped m when m.Name.EndsWith("IExports") -> 
                    match findByName typeMap m.Name with 
                    | Some t -> { Name = export.Name; Type' = getTypeSignature typeMap t }
                    | _ -> failwith "Type Unknown"
                | t -> { Name = export.Name; Type' = getTypeSignature typeMap t }
            )
    
    