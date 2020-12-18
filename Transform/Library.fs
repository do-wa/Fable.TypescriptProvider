module rec Transform

open ts2fable
open ts2fable.Syntax
open System
open System.Collections.Generic
open ProviderImplementation.ProvidedTypes


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

let private primitives = ["bool"; "int"; "float"; "string"; "unit"; "obj"] |> Set.ofList

let private find (types: FsType seq) (matcher: FsType -> 'a option) = 
    types |> Seq.tryPick matcher

let rec private findByName (types: Dictionary<string, FsType>) name = 
    match types.TryGetValue name with 
    | false, _ when name.Contains(".") -> 
        findByName types (name.Split('.') |> Array.tail |> String.concat " ")
    | false, _ -> 
        match types.Keys |> Seq.tryFind(fun x -> x.EndsWith name) with 
        | Some fb -> findByName types fb
        | None -> None
    | true, v -> Some v

let private findByType (matching: FsType) = Seq.tryFind(fun x -> x = matching)

type ParentInfo = {
    Name: string
    IsOptional: bool
}
    // Parents, Type, TypeRelevantProps

type ErasedType =
    | Any of string
    | Bool of string
    | Int of string
    | Float of string
    | String of string
    | Array of ErasedType
    | Option of ErasedType
    | Custom of System.Type
    | Fn of string * ErasedType list * ErasedType * FsFunctionKind
    | Type of string * ErasedType list   
    | ReactComponent of ErasedType option
    | Inherits of ErasedType * ErasedType


type MappedType =
    | React of {| Member : string |}
    | Primitive of {| Name: string; TypeName : string; IsOptional: bool  |}
    | Property of {| Parents: (ParentInfo list); Source: FsProperty; Mapped:  MappedType |}
    | Function of {| Parents: (ParentInfo list); Source: FsFunction; Args: ((FsParam * MappedType) list); Ret: MappedType  |}
    | IFace of {| Parents: (ParentInfo list); Inherits: (MappedType list); Source: FsInterface; Props: (MappedType list) |}
    | Generic of {| TypeParam : MappedType; TypeArgs:(MappedType list) |}
    | NotMapped of string



let rec private getResolvedType (typeMap: Dictionary<string, FsType>) (parents: ParentInfo list) (fsType: FsType) =
    let fromParent defVal get =
        match parents with 
        | x::_ -> get x
        | _ -> defVal

    match fsType with 
    | FsType.Import(FsImport.Type(t)) -> 
        match findByName typeMap t.ImportSpecifier.Name with
        | None when t.SpecifiedModule.StartsWith "React." -> React({| Member = t.ImportSpecifier.Name |})
        | None -> failwith (sprintf "Could not find %s type" t.ImportSpecifier.Name)
        | Some t -> match t with 
                    | FsType.Import(FsImport.Type(t)) when t.SpecifiedModule = "React" -> React({| Member = t.ImportSpecifier.Name |})
                    | _ -> failwith "Unexpected import"
    | FsType.Generic g -> 
        let t = getResolvedType typeMap parents g.Type
        let tParams =  g.TypeParameters |> List.map (getResolvedType typeMap parents)
        MappedType.Generic({| TypeParam = t; TypeArgs = tParams |})
    | FsType.Mapped m when primitives.Contains(m.Name) -> MappedType.Primitive({| Name = fromParent "" (fun x -> x.Name); TypeName = m.Name; IsOptional = fromParent false (fun x -> x.IsOptional)  |})
    | FsType.Mapped m ->
        match findByName typeMap m.Name with
        | None when m.Name.StartsWith "React." -> React({| Member = m.Name |})
        | None -> failwith (sprintf "Could not find %s type" m.Name)
        | Some t -> getResolvedType typeMap parents t
    | FsType.Interface i ->    
        let heritage = i.Inherits |> List.map (getResolvedType typeMap [])
        let members = i.Members |> List.map (getResolvedType typeMap (({ Name = i.Name; IsOptional = false })::parents))
        MappedType.IFace({|Parents = parents; Inherits = heritage; Source= i; Props = members|})
    | FsType.Property p -> 
        let root = getResolvedType typeMap parents p.Type
        MappedType.Property({| Parents = ({Name = p.Name; IsOptional = p.Option})::parents; Source = p; Mapped = root |})
    | FsType.Function f -> 
        let args = f.Params |> List.map(fun p -> p, getResolvedType typeMap (({Name = p.Name; IsOptional = p.Optional})::parents )p.Type)
        let ret = f.ReturnType |> getResolvedType typeMap []
        MappedType.Function({| Parents = parents; Source = f; Args = args; Ret= ret |})
    | _ -> MappedType.NotMapped((sprintf "%A" fsType))

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


let getTypeSignature (typeMap: Dictionary<string, FsType>) =
    let getExportedVariable = find typeMap.Values (function | FsType.Variable v -> Some v | _ -> None)
    match getExportedVariable with 
    | None -> failwith "Variable expected"
    | Some v -> 
            match v.Type with 
            | FsType.Mapped m when m.Name.EndsWith("IExports") -> 
                match findByName typeMap m.Name with 
                | Some(FsType.Interface i) ->
                    let signature = i.Members |> List.map (getResolvedType typeMap [{ Name = i.Name; IsOptional = false }])
                    v, i, signature
                | _ -> failwith "NotYetSupported"
            | _ -> failwith "NotYetSupported"
   

let rec private mapMappedTypeToErasedType (mappedType: MappedType) =
    match mappedType with
    | React r when r.Member = "Component" -> ErasedType.ReactComponent(None)
    | Primitive p ->
        match p.TypeName,p.IsOptional with 
        | "string", true -> ErasedType.Option(ErasedType.String(p.Name))
        | "string", false -> ErasedType.String(p.Name)
        | "int", true -> ErasedType.Option(ErasedType.Int(p.Name))
        | "int", false -> ErasedType.Int(p.Name)
        | "float", true -> ErasedType.Option(ErasedType.Float(p.Name))
        | "float", false -> ErasedType.Float(p.Name)
        | "bool", true -> ErasedType.Option(ErasedType.Bool(p.Name))
        | "bool", false -> ErasedType.Float(p.Name)
        | _ , _ -> ErasedType.String(p.Name)
    | Generic g -> 
        match g.TypeParam |> mapMappedTypeToErasedType  with 
        | ReactComponent _ -> 
            let props = g.TypeArgs |> List.tryHead |> Option.map mapMappedTypeToErasedType
            ErasedType.ReactComponent(props)
        | t -> t
    | Function f ->
        let ret = mapMappedTypeToErasedType f.Ret
        let p = f.Args 
                |> List.map(fun (p,mt) -> mapMappedTypeToErasedType mt)
        ErasedType.Fn((Option.defaultValue "" f.Source.Name ), p, ret, f.Source.Kind)
    | IFace i ->
        let heritage = i.Inherits 
                             |> List.map mapMappedTypeToErasedType
        let p = i.Props 
                |> List.map mapMappedTypeToErasedType
        let t = ErasedType.Type(i.Source.Name, p)
        if heritage.Length = 0
        then t
        elif heritage.Length = 1 
        then ErasedType.Inherits(t, heritage.Head)
        else failwith "Support chains"
    | _ -> ErasedType.String(Guid.NewGuid().ToString().Replace("-",""))


let rec private mapToProvidedType (parentType: ProvidedTypeDefinition) (provideTo: string * Type -> unit) (mappedType: MappedType)  =
    match mappedType with
    | Primitive p ->
        match p.TypeName,p.IsOptional with 
        | "string", true -> 
            provideTo(p.Name, typedefof<Option<obj>>.MakeGenericType(typeof<string>))
        | "string", false -> 
            provideTo(p.Name, typeof<string>)
        | "int", true -> 
            provideTo(p.Name, typedefof<Option<obj>>.MakeGenericType(typeof<int>))
        | "int", false -> 
            provideTo(p.Name, typeof<int>)
        | "float", true -> 
            provideTo(p.Name, typedefof<Option<obj>>.MakeGenericType(typeof<float>))
        | "float", false -> 
            provideTo(p.Name, typeof<float>)
        | "bool", true -> 
            provideTo(p.Name, typedefof<Option<obj>>.MakeGenericType(typeof<bool>))
        | "bool", false ->
           provideTo(p.Name, typeof<bool>)
        | "unit", _ ->
           provideTo(p.Name, typeof<unit>)
        | _ , _ -> 
           provideTo(Guid.NewGuid().ToString().Replace("-",""), typeof<string>)
        parentType
    | Function f ->
        let fParams = ResizeArray<ProvidedParameter>()
        let mutable retType : Type = typeof<obj>
        let addToFn = fun (n,t) -> fParams.Add(ProvidedParameter(n,t))
        f.Ret |> (mapToProvidedType parentType (fun (n,t) -> retType <- t) >> ignore)
        let fn = ProviderDsl.makeNoInvokeMethod(Option.defaultValue "Create" f.Source.Name, (fParams.ToArray() |> Array.toList), retType, false)
        parentType.AddMember(fn)
        
        parentType
    | IFace i ->
        let newType = ProviderDsl.makeType(i.Source.Name, false)
        let addToType = fun (n,t) -> newType.AddMember(ProviderDsl.makeProperty(n,t,false))
        i.Props |> List.iter ((mapToProvidedType newType addToType) >> ignore )
        parentType.AddMember(newType :> Type)
        parentType
    | _ -> parentType


let (|ProvidedReactComponent|_|) (mappedType: MappedType) = 
    match mappedType with 
        | Generic g -> 
            match g.TypeParam with 
            | IFace comp -> 
                match comp.Inherits |> List.tryHead with
                | Some(Generic reactComponent) ->
                    match reactComponent.TypeParam with
                    | React m when m.Member = "Component" -> 
                        match reactComponent.TypeArgs.Head with
                        | IFace props -> Some (IFace props)
                        | _ -> None
                    | _ -> None
                | _ -> None
            | _ -> None
        | _ -> None
       

let toProvidedTypes (rootType: ProvidedTypeDefinition) exportProps = 
    // these are the entry points
    exportProps 
    |> List.collect(function
                | MappedType.Property p ->   
                     let propertyName = p.Source.Name
                     // extract mapped type
                     match p.Mapped with 
                     | MappedType.IFace i ->
                        // its mapped to an type
                        // we analyse the type properties and have following scenarios
                        // 1. The type has an constructor so we look at the return type of the ctor to determine the actual type
                        let returnType = 
                            match i.Source.HasConstructor with 
                            | true -> 
                                let ctor = i.Props.Head // TODO: make this right
                                match ctor with 
                                | MappedType.Function ctorFunction -> 
                                    match ctorFunction.Ret with 
                                    | ProvidedReactComponent(IFace m) ->   
                                        let propType = ProviderDsl.makeType(m.Source.Name, true)
                                        // TODO: Create FActory Fn to instantiate Props
                                        m.Props |> List.iter ((mapToProvidedType propType (fun (n,t) -> propType.AddMember(ProviderDsl.makeProperty(n,t,false)))) >> ignore)
                                        propType
                                | _ -> failwith "Expected Ctr to be a function"
                            | _ -> failwith "Expected Ctor at the moment"
                        []
                | _ -> failwith "Not supported yet"
    )

let mapToErasableType exportProps = 
    // these are the entry points
    exportProps 
    |> List.collect(function
                | MappedType.Function f ->
                         let props = f.Args |> List.map (fun (_,m) -> mapMappedTypeToErasedType m)
                         let ret = f.Ret |> mapMappedTypeToErasedType
                         [ErasedType.Fn(Option.defaultValue "" f.Source.Name, props, ret, f.Source.Kind)]
                | MappedType.Property p ->   
                     match p.Mapped with 
                     | MappedType.IFace i ->
                        i.Props
                        |> List.map(function 
                                    | MappedType.Function f -> 
                                            let props = f.Args |> List.map (fun (_,m) -> mapMappedTypeToErasedType m)
                                            let ret = f.Ret |> mapMappedTypeToErasedType
                                            ErasedType.Fn(Option.defaultValue "" f.Source.Name, props, ret, f.Source.Kind)
                                        | _ -> failwith "Not supported" )
                                    | _ -> failwith "bla"

                | _ -> failwith "Not supported yet"
    )