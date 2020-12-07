namespace Fable.Core

open ts2fable

open FSharp.Compiler.Text
open System.Diagnostics

type EmitAttribute(macro: string) =
    inherit System.Attribute()

namespace Fable

module TypescriptProvider =
    open System
    open System.IO
    open System.Net 
    open System.Text.RegularExpressions
    open FSharp.Quotations
    open FSharp.Core.CompilerServices
    open ProviderImplementation.ProvidedTypes
    open System.Diagnostics
    open ts2fable.Syntax
    open ProviderDsl
    open Fable.Core

    // TODO: Interop between ts2fable and TypeProvider 
    // The ts2fable package is adjusted to return .json files with the type information instead of the .fs file
    let sample = """{"Namespace":"index","Opens":["System","Fable.Core","Fable.Core.JS"],"Files":[{"Kind":"Index","FileName":"C:/Users/dominik/Projects/FSharp.Fable.TypescriptProvider/sample/Simple/node_modules/left-pad/index.d.ts","ModuleName":"left-pad","Modules":[{"HasDeclare":false,"IsNamespace":false,"Name":"","Types":[["Variable",{"Export":{"IsGlobal":false,"Selector":"*","Path":"left-pad"},"HasDeclare":true,"Name":"leftPad","Type":["Mapped",{"Name":"LeftPad.IExports","FullName":"LeftPad.IExports"}],"IsConst":true,"IsStatic":false,"Accessibility":null}],["Interface",{"Comments":[],"IsStatic":false,"IsClass":false,"Name":"IExports","FullName":"IExports","TypeParameters":[],"Inherits":[],"Members":[["Function",{"Comments":[],"Kind":"Regular","IsStatic":false,"Name":"leftPad","TypeParameters":[],"Params":[{"Comment":null,"Name":"str","Optional":false,"ParamArray":false,"Type":["Union",{"Option":false,"Types":[["Mapped",{"Name":"string","FullName":"string"}],["Mapped",{"Name":"float","FullName":"float"}]]}]},{"Comment":null,"Name":"len","Optional":false,"ParamArray":false,"Type":["Mapped",{"Name":"float","FullName":"float"}]},{"Comment":null,"Name":"ch","Optional":true,"ParamArray":false,"Type":["Union",{"Option":false,"Types":[["Mapped",{"Name":"string","FullName":"string"}],["Mapped",{"Name":"float","FullName":"float"}]]}]}],"ReturnType":["Mapped",{"Name":"string","FullName":"string"}],"Accessibility":null}]],"Accessibility":null}],["Module",{"HasDeclare":true,"IsNamespace":true,"Name":"LeftPad","Types":[],"HelperLines":[],"Attributes":[]}],["ExportAssignment","leftPad"]],"HelperLines":[],"Attributes":[]}]}],"AbbrevTypes":[]}"""
    let fromProvidedOrDefault (default': string) (provided: ProvidedTypeDefinition option) = if provided = None then default' else provided.Value.Name
   
    open Thoth.Json.Net
    open ts2fable.Syntax
    
    // TODO: make this dict method all simpler
    let getAndCacheType (forceValueUpdate :bool) (types: Collections.Generic.Dictionary<string, ProvidedTypeDefinition>) (name: string) (getType: string -> ProvidedTypeDefinition)  =
        if forceValueUpdate = true 
        then 
            let type' = getType name
            types.[name] <- type'

        match types.TryGetValue name with
        | false, _ -> 
            let type' = getType name
            types.[name] <- type'
            type'
        | true, type' -> type'
    let getOrCreateType = getAndCacheType false
    let getProvidedType  (types: Collections.Generic.Dictionary<string, ProvidedTypeDefinition>) (name: string) = 
        match types.TryGetValue name with
        | false, _ -> None
        | true, type' -> Some type'

    let mapType (types: Collections.Generic.Dictionary<string, ProvidedTypeDefinition>) (typeName:string) (isOption: bool) =
        let customType = getProvidedType types typeName
        let erasedType = 
            match customType with
            | Some t -> ErasedType.Custom(t)
            | None -> 
            match typeName with 
                      | "string" when not isOption -> ErasedType.String
                      | "string" when isOption -> ErasedType.Option(ErasedType.String)
                      | "int" when not isOption -> ErasedType.Int
                      | "int" when isOption -> ErasedType.Option(ErasedType.Int)
                      | "float" when not isOption -> ErasedType.Float
                      | "float" when isOption -> ErasedType.Option(ErasedType.Float)
                      | "bool" when not isOption -> ErasedType.Bool
                      | "bool" when isOption -> ErasedType.Option(ErasedType.Bool)
                      | _ -> ErasedType.String // Todo add Array and others
        erasedType

    let rec toType (types: Collections.Generic.Dictionary<string, ProvidedTypeDefinition>) (parentType: ProvidedTypeDefinition option)  = function 
        | ts2fable.Syntax.FsType.Enum enum -> 
            let enumBaseType = 
                match parentType with
                | None -> getOrCreateType types enum.Name (fun name -> ProvidedTypeDefinition(name, None)) 
                | Some t -> getAndCacheType true types t.Name (fun name -> ProvidedTypeDefinition(name, None)) 
            match enum.Type with 
            | FsEnumCaseType.Numeric ->
                enumBaseType.SetEnumUnderlyingType(typeof<float>)
                enum.Cases 
                |> List.iteri(fun i case -> enumBaseType.AddMember(ProvidedField.Literal(case.Name, enumBaseType, Convert.ToDouble (Option.defaultValue (i.ToString()) case.Value))))
            | FsEnumCaseType.String
            | FsEnumCaseType.Unknown ->
                enumBaseType.SetEnumUnderlyingType(typeof<string>)
                enum.Cases 
                |> List.iteri(fun i case -> enumBaseType.AddMember(ProvidedField.Literal(case.Name, enumBaseType, Option.defaultValue (case.Name+i.ToString()) case.Value)))
            [enumBaseType]
        | ts2fable.Syntax.FsType.Interface interface' -> 
            let interfaceType = getOrCreateType types interface'.Name (fun n -> ProviderDsl.makeInterfaceType(n)) 
            interface'.Members
            |> List.collect (toType types (Some interfaceType))
        | ts2fable.Syntax.FsType.Property prop when parentType <> None -> 
            match prop.Type with 
            | ts2fable.Syntax.FsType.Mapped mapped -> 
                let mappedType = mapType types mapped.Name prop.Option
                parentType.Value.AddMember(ProviderDsl.makeProperty(prop.Name, mappedType))
                [parentType.Value]
            | _ ->  [parentType.Value]
        | ts2fable.Syntax.FsType.Function func -> 
            match parentType with 
            | None -> failwith "Not supported" // TODO: support top level function - or is this irrelevant?
            | Some parentType -> 
                let params' = 
                    func.Params 
                    |> List.map(fun param -> 
                        let name = param.Name
                        match param.Type with 
                        | ts2fable.Syntax.FsType.Mapped mapped -> 
                            let mappedType = mapType types mapped.Name param.Optional
                            name, mappedType
                        | _ ->  "", ErasedType.String // TODO: support other types
                    )
                let method = ProviderDsl.makeMethod(Option.defaultValue "Invoke" func.Name, params', ErasedType.String) // TODO: support return type
                parentType.AddMember(method)
                [parentType]

        | _ -> [ProviderDsl.makeInterfaceType(System.Guid.NewGuid().ToString().Replace("-",""))] // TODO: unions, tuples, generics... what else?
    
    [<TypeProvider>]
    type public TypescriptProvider (config : TypeProviderConfig) as this =
        inherit TypeProviderForNamespaces (config)
        let asm = System.Reflection.Assembly.GetExecutingAssembly()
        let ns = "Fable.TypescriptProvider"

        let staticParams = [ProvidedStaticParameter("module",typeof<string>)]
        // TODO rename generator (borrowed from JsonProvider). 
        // Add Options to add additional "render" method into type for direct react component import (main motivation for this project)
        let generator = ProvidedTypeDefinition(asm, ns, "Generator", Some typeof<obj>, isErased = true)

        do generator.DefineStaticParameters(
            parameters = staticParams,
            instantiationFunction = (fun typeName pVals ->
                    match pVals with
                    | [| :? string as arg|] ->
                        // TODO: import d.ts file from filesystem, call ts2fable 
                        match Decode.Auto.fromString<ts2fable.Syntax.FsFileOut> sample with
                        | Error err -> failwith err
                        | Ok tsType -> 
                            let typeMap = Collections.Generic.Dictionary<string, ProvidedTypeDefinition>()
                            let subTypes = 
                                tsType.Files
                                |> List.map(
                                    fun f -> ProviderDsl.makeInterfaceTypeWithMembers(
                                                f.ModuleName, 
                                                f.Modules |> List.collect(fun m -> 
                                                    if m.Name <> f.ModuleName && String.IsNullOrEmpty(m.Name) = false
                                                    then [ ProviderDsl.makeInterfaceTypeWithMembers(m.Name, m.Types |> List.collect (toType typeMap None)) ]
                                                    else m.Types |> List.collect (toType typeMap None))))
                            let root = makeRootType(asm, ns, typeName, subTypes)
                            
                            
                            root
                    | _ -> failwith "unexpected parameter values"
                )
            )

        do this.AddNamespace(ns, [generator])

    [<assembly:TypeProviderAssembly>]
    do ()