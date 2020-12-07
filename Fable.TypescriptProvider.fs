﻿namespace Fable.Core

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

    let sample = """{"Namespace":"test2","Opens":["System","Fable.Core","Fable.Core.JS","Browser.Types"],"Files":[{"Kind":"Index","FileName":"../test.d.ts","ModuleName":"test","Modules":[{"HasDeclare":false,"IsNamespace":false,"Name":"","Types":[["Interface",{"Comments":[],"IsStatic":false,"IsClass":false,"Name":"IExports","FullName":"IExports","TypeParameters":[],"Inherits":[],"Members":[["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"ReactTooltip","Option":false,"Type":["Mapped",{"Name":"ReactTooltipStatic","FullName":"ReactTooltipStatic"}],"IsReadonly":true,"IsStatic":false,"Accessibility":null}]],"Accessibility":null}],["Import",["Module",{"Module":"React","SpecifiedModule":"React","ResolvedModule":null}]],["Interface",{"Comments":[],"IsStatic":false,"IsClass":false,"Name":"Offset","FullName":"","TypeParameters":[],"Inherits":[],"Members":[["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"top","Option":true,"Type":["Mapped",{"Name":"float","FullName":"float"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"right","Option":true,"Type":["Mapped",{"Name":"float","FullName":"float"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"left","Option":true,"Type":["Mapped",{"Name":"float","FullName":"float"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"bottom","Option":true,"Type":["Mapped",{"Name":"float","FullName":"float"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}]],"Accessibility":null}],["Interface",{"Comments":[],"IsStatic":false,"IsClass":false,"Name":"Place","FullName":"Place","TypeParameters":[],"Inherits":[],"Members":[["Enum",{"Name":"StringEnum","Cases":[{"Name":"top","Type":"String","Value":"top"},{"Name":"right","Type":"String","Value":"right"},{"Name":"bottom","Type":"String","Value":"bottom"},{"Name":"left","Type":"String","Value":"left"}]}]],"Accessibility":null}],["Interface",{"Comments":[],"IsStatic":false,"IsClass":false,"Name":"Type","FullName":"Type","TypeParameters":[],"Inherits":[],"Members":[["Enum",{"Name":"StringEnum","Cases":[{"Name":"dark","Type":"String","Value":"dark"},{"Name":"success","Type":"String","Value":"success"},{"Name":"warning","Type":"String","Value":"warning"},{"Name":"error","Type":"String","Value":"error"},{"Name":"info","Type":"String","Value":"info"},{"Name":"light","Type":"String","Value":"light"}]}]],"Accessibility":null}],["Interface",{"Comments":[],"IsStatic":false,"IsClass":false,"Name":"Effect","FullName":"Effect","TypeParameters":[],"Inherits":[],"Members":[["Enum",{"Name":"StringEnum","Cases":[{"Name":"float","Type":"String","Value":"float"},{"Name":"solid","Type":"String","Value":"solid"}]}]],"Accessibility":null}],["Interface",{"Comments":[],"IsStatic":false,"IsClass":false,"Name":"VoidFunc","FullName":"VoidFunc","TypeParameters":[],"Inherits":[],"Members":[["Function",{"Comments":[],"Kind":"Call","IsStatic":false,"Name":"Invoke","TypeParameters":[],"Params":[{"Comment":null,"Name":"args","Optional":false,"ParamArray":true,"Type":["Array",["Union",{"Option":true,"Types":[["Mapped",{"Name":"obj","FullName":"obj"}]]}]]}],"ReturnType":["Mapped",{"Name":"unit","FullName":"unit"}],"Accessibility":null}]],"Accessibility":null}],["Interface",{"Comments":[],"IsStatic":false,"IsClass":false,"Name":"GetContentFunc","FullName":"GetContentFunc","TypeParameters":[],"Inherits":[],"Members":[["Function",{"Comments":[],"Kind":"Call","IsStatic":false,"Name":"Invoke","TypeParameters":[],"Params":[{"Comment":null,"Name":"toolTipStr","Optional":false,"ParamArray":false,"Type":["Mapped",{"Name":"string","FullName":"string"}]}],"ReturnType":["Mapped",{"Name":"React.ReactNode","FullName":""}],"Accessibility":null}]],"Accessibility":null}],["Alias",{"Name":"GetContent","Type":["Union",{"Option":false,"Types":[["Mapped",{"Name":"GetContentFunc","FullName":""}],["Tuple",{"Types":[["Mapped",{"Name":"GetContentFunc","FullName":""}],["Mapped",{"Name":"float","FullName":"float"}]],"Kind":"Tuple"}]]}],"TypeParameters":[]}],["Interface",{"Comments":[],"IsStatic":false,"IsClass":false,"Name":"TooltipProps","FullName":"","TypeParameters":[],"Inherits":[],"Members":[["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"children","Option":true,"Type":["Mapped",{"Name":"React.ReactNode","FullName":""}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"uuid","Option":true,"Type":["Mapped",{"Name":"string","FullName":"string"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"place","Option":true,"Type":["Mapped",{"Name":"Place","FullName":""}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"``type``","Option":true,"Type":["Mapped",{"Name":"Type","FullName":""}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"effect","Option":true,"Type":["Mapped",{"Name":"Effect","FullName":""}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"offset","Option":true,"Type":["Mapped",{"Name":"Offset","FullName":""}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"multiline","Option":true,"Type":["Mapped",{"Name":"bool","FullName":"bool"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"border","Option":true,"Type":["Mapped",{"Name":"bool","FullName":"bool"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"textColor","Option":true,"Type":["Mapped",{"Name":"string","FullName":"string"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"backgroundColor","Option":true,"Type":["Mapped",{"Name":"string","FullName":"string"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"borderColor","Option":true,"Type":["Mapped",{"Name":"string","FullName":"string"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"arrowColor","Option":true,"Type":["Mapped",{"Name":"string","FullName":"string"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"insecure","Option":true,"Type":["Mapped",{"Name":"bool","FullName":"bool"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"``class``","Option":true,"Type":["Mapped",{"Name":"string","FullName":"string"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"className","Option":true,"Type":["Mapped",{"Name":"string","FullName":"string"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"id","Option":true,"Type":["Mapped",{"Name":"string","FullName":"string"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"html","Option":true,"Type":["Mapped",{"Name":"bool","FullName":"bool"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"delayHide","Option":true,"Type":["Mapped",{"Name":"float","FullName":"float"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"delayUpdate","Option":true,"Type":["Mapped",{"Name":"float","FullName":"float"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"delayShow","Option":true,"Type":["Mapped",{"Name":"float","FullName":"float"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"``event``","Option":true,"Type":["Mapped",{"Name":"string","FullName":"string"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"eventOff","Option":true,"Type":["Mapped",{"Name":"string","FullName":"string"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"isCapture","Option":true,"Type":["Mapped",{"Name":"bool","FullName":"bool"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"globalEventOff","Option":true,"Type":["Mapped",{"Name":"string","FullName":"string"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"getContent","Option":true,"Type":["Mapped",{"Name":"GetContent","FullName":""}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"afterShow","Option":true,"Type":["Mapped",{"Name":"VoidFunc","FullName":""}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"afterHide","Option":true,"Type":["Mapped",{"Name":"VoidFunc","FullName":""}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"overridePosition","Option":true,"Type":["Function",{"Comments":[],"Kind":"Regular","IsStatic":false,"TypeParameters":[],"Params":[{"Comment":null,"Name":"position","Optional":false,"ParamArray":false,"Type":["Mapped",{"Name":"TooltipPropsOverridePosition","FullName":"TooltipPropsOverridePosition"}]},{"Comment":null,"Name":"currentEvent","Optional":false,"ParamArray":false,"Type":["Mapped",{"Name":"Event","FullName":""}]},{"Comment":null,"Name":"currentTarget","Optional":false,"ParamArray":false,"Type":["Mapped",{"Name":"EventTarget","FullName":""}]},{"Comment":null,"Name":"refNode","Optional":false,"ParamArray":false,"Type":["Union",{"Option":true,"Types":[["Mapped",{"Name":"HTMLDivElement","FullName":""}],["Mapped",{"Name":"HTMLSpanElement","FullName":""}]]}]},{"Comment":null,"Name":"place","Optional":false,"ParamArray":false,"Type":["Mapped",{"Name":"Place","FullName":""}]},{"Comment":null,"Name":"desiredPlace","Optional":false,"ParamArray":false,"Type":["Mapped",{"Name":"Place","FullName":""}]},{"Comment":null,"Name":"effect","Optional":false,"ParamArray":false,"Type":["Mapped",{"Name":"Effect","FullName":""}]},{"Comment":null,"Name":"offset","Optional":false,"ParamArray":false,"Type":["Mapped",{"Name":"Offset","FullName":""}]}],"ReturnType":["Mapped",{"Name":"TooltipPropsOverridePosition","FullName":"TooltipPropsOverridePosition"}],"Accessibility":null}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"disable","Option":true,"Type":["Mapped",{"Name":"bool","FullName":"bool"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"scrollHide","Option":true,"Type":["Mapped",{"Name":"bool","FullName":"bool"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"resizeHide","Option":true,"Type":["Mapped",{"Name":"bool","FullName":"bool"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"wrapper","Option":true,"Type":["Mapped",{"Name":"TooltipPropsWrapper","FullName":"TooltipPropsWrapper"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"bodyMode","Option":true,"Type":["Mapped",{"Name":"bool","FullName":"bool"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"possibleCustomEvents","Option":true,"Type":["Mapped",{"Name":"string","FullName":"string"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"possibleCustomEventsOff","Option":true,"Type":["Mapped",{"Name":"string","FullName":"string"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"clickable","Option":true,"Type":["Mapped",{"Name":"bool","FullName":"bool"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}]],"Accessibility":null}],["Interface",{"Comments":[],"IsStatic":false,"IsClass":true,"Name":"ReactTooltip","FullName":"","TypeParameters":[],"Inherits":[["Generic",{"Type":["Mapped",{"Name":"React.Component","FullName":"error"}],"TypeParameters":[["Mapped",{"Name":"TooltipProps","FullName":""}]]}]],"Members":[],"Accessibility":null}],["Interface",{"Comments":[],"IsStatic":true,"IsClass":true,"Name":"ReactTooltipStatic","FullName":"","TypeParameters":[],"Inherits":[],"Members":[["Function",{"Comments":[],"Kind":"Constructor","IsStatic":true,"Name":"Create","TypeParameters":[],"Params":[],"ReturnType":["Generic",{"Type":["Mapped",{"Name":"ReactTooltip","FullName":"ReactTooltip"}],"TypeParameters":[]}],"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"show","Option":false,"Type":["Function",{"Comments":[],"Kind":"Regular","IsStatic":false,"TypeParameters":[],"Params":[{"Comment":null,"Name":"target","Optional":false,"ParamArray":false,"Type":["Mapped",{"Name":"Element","FullName":""}]}],"ReturnType":["Mapped",{"Name":"ReactTooltipStaticShow","FullName":"ReactTooltipStaticShow"}],"Accessibility":null}],"IsReadonly":false,"IsStatic":true,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"hide","Option":false,"Type":["Function",{"Comments":[],"Kind":"Regular","IsStatic":false,"TypeParameters":[],"Params":[{"Comment":null,"Name":"target","Optional":true,"ParamArray":false,"Type":["Mapped",{"Name":"Element","FullName":""}]}],"ReturnType":["Mapped",{"Name":"ReactTooltipStaticShow","FullName":"ReactTooltipStaticShow"}],"Accessibility":null}],"IsReadonly":false,"IsStatic":true,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"rebuild","Option":false,"Type":["Function",{"Comments":[],"Kind":"Regular","IsStatic":false,"TypeParameters":[],"Params":[],"ReturnType":["Mapped",{"Name":"ReactTooltipStaticShow","FullName":"ReactTooltipStaticShow"}],"Accessibility":null}],"IsReadonly":false,"IsStatic":true,"Accessibility":null}]],"Accessibility":null}],["Interface",{"Comments":[],"IsStatic":false,"IsClass":false,"Name":"TooltipPropsOverridePosition","FullName":"TooltipPropsOverridePosition","TypeParameters":[],"Inherits":[],"Members":[["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"left","Option":false,"Type":["Mapped",{"Name":"float","FullName":"float"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}],["Property",{"Comments":[],"Kind":"Regular","Index":null,"Name":"top","Option":false,"Type":["Mapped",{"Name":"float","FullName":"float"}],"IsReadonly":false,"IsStatic":false,"Accessibility":null}]],"Accessibility":null}],["Interface",{"Comments":[],"IsStatic":false,"IsClass":false,"Name":"TooltipPropsWrapper","FullName":"TooltipPropsWrapper","TypeParameters":[],"Inherits":[],"Members":[["Enum",{"Name":"StringEnum","Cases":[{"Name":"div","Type":"String","Value":"div"},{"Name":"span","Type":"String","Value":"span"}]}]],"Accessibility":null}],["Interface",{"Comments":[],"IsStatic":false,"IsClass":false,"Name":"ReactTooltipStaticShow","FullName":"ReactTooltipStaticShow","TypeParameters":[],"Inherits":[],"Members":[],"Accessibility":null}]],"HelperLines":[],"Attributes":[]}]}],"AbbrevTypes":[]}"""
    let fromProvidedOrDefault (default': string) (provided: ProvidedTypeDefinition option) = if provided = None then default' else provided.Value.Name
   
    open Thoth.Json.Net
    open ts2fable.Syntax
    
    
    let getOrCreateType (types: Collections.Generic.Dictionary<string, ProvidedTypeDefinition>) (name: string) (getType: string -> ProvidedTypeDefinition) =
        match types.TryGetValue name with
        | false, _ -> 
            let type' = getType name
            types.[name] <- type'
            type'
        | true, type' -> type'
    
    let getType  (types: Collections.Generic.Dictionary<string, ProvidedTypeDefinition>) (name: string) = 
        match types.TryGetValue name with
        | false, _ -> None
        | true, type' -> Some type'



    let rec toType (types: Collections.Generic.Dictionary<string, ProvidedTypeDefinition>) (parentName: string option)  = function 
        | ts2fable.Syntax.FsType.Enum enum -> 
            let enumBaseType = getOrCreateType types (Option.defaultValue enum.Name parentName) (fun name -> ProvidedTypeDefinition(name, None))
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
            ChildType(enumBaseType)
        | ts2fable.Syntax.FsType.Interface interface' -> 
            let name = interface'.Name
            if interface'.Members.Length = 1 && interface'.Members |> List.forall(function | FsType.Enum _ -> true | _ -> false)
            then 
                interface'.Members.Head |> toType types (Some name)
            else 
                let interfaceType = getOrCreateType types name (fun n -> ProvidedTypeDefinition(n , baseType = Some typeof<obj>, hideObjectMethods = true, isErased = true, isInterface = true))
                ProviderDsl.ChildType(ProviderDsl.makeCustomType(name, interface'.Members |> List.map (toType types (Some name)), true ))
                //interfaceType.AddMembers(
                //    interface'.Members 
                //    |> List.map(function 
                //                | ts2fable.Syntax.FsType.Property prop -> 
                //                    match prop.Type with 
                //                    | ts2fable.Syntax.FsType.Mapped mapped -> 
                //                        let customType = getType types mapped.Name
                //                        let erasedType = 
                //                            match customType with
                //                            | Some t -> ErasedType.Custom(t)
                //                            | None -> 
                //                            match mapped.Name with 
                //                                      | "string" when not prop.Option -> ErasedType.String
                //                                      | "string" when prop.Option -> ErasedType.Option(ErasedType.String)
                //                                      | "int" when not prop.Option -> ErasedType.Int
                //                                      | "int" when prop.Option -> ErasedType.Option(ErasedType.Int)
                //                                      | "float" when not prop.Option -> ErasedType.Float
                //                                      | "float" when prop.Option -> ErasedType.Option(ErasedType.Float)
                //                                      | "bool" when not prop.Option -> ErasedType.Bool
                //                                      | "bool" when prop.Option -> ErasedType.Option(ErasedType.Bool)
                //                                      | _ -> ErasedType.String // Todo add Array and others
                //                        ProvidedProperty(prop.Name, makeType erasedType, (fun args -> args.[0]))
                //                    | _ ->  ProvidedProperty(prop.Name, makeType(ErasedType.String), (fun args -> args.[0]))
                //                | _ -> ProvidedProperty("test", makeType(ErasedType.Bool), (fun args -> args.[0]))
                //    ))
                //ChildType(interfaceType)
        | ts2fable.Syntax.FsType.Property prop -> 
            match prop.Type with 
            | ts2fable.Syntax.FsType.Mapped mapped -> 
                let customType = getType types mapped.Name
                let erasedType = 
                    match customType with
                    | Some t -> ErasedType.Custom(t)
                    | None -> 
                    match mapped.Name with 
                              | "string" when not prop.Option -> ErasedType.String
                              | "string" when prop.Option -> ErasedType.Option(ErasedType.String)
                              | "int" when not prop.Option -> ErasedType.Int
                              | "int" when prop.Option -> ErasedType.Option(ErasedType.Int)
                              | "float" when not prop.Option -> ErasedType.Float
                              | "float" when prop.Option -> ErasedType.Option(ErasedType.Float)
                              | "bool" when not prop.Option -> ErasedType.Bool
                              | "bool" when prop.Option -> ErasedType.Option(ErasedType.Bool)
                              | _ -> ErasedType.String // Todo add Array and others
                ProviderDsl.Property(prop.Name, erasedType)
            | _ ->   ProviderDsl.Property(prop.Name, ErasedType.String)
        | _ -> ProviderDsl.ChildType(makeCustomType("_innerModType0", [], false))
    
    [<TypeProvider>]
    type public TypescriptProvider (config : TypeProviderConfig) as this =
        inherit TypeProviderForNamespaces (config)
        let asm = System.Reflection.Assembly.GetExecutingAssembly()
        let ns = "Fable.TypescriptProvider"

        let staticParams = [ProvidedStaticParameter("module",typeof<string>)]
        let generator = ProvidedTypeDefinition(asm, ns, "Generator", Some typeof<obj>, isErased = true)

        do generator.DefineStaticParameters(
            parameters = staticParams,
            instantiationFunction = (fun typeName pVals ->
                    match pVals with
                    | [| :? string as arg|] ->
                        match Decode.Auto.fromString<ts2fable.Syntax.FsFileOut> sample with
                        | Error err -> failwith err
                        | Ok tsType -> 
                            let typeMap = Collections.Generic.Dictionary<string, ProvidedTypeDefinition>()
                            let typeFactory = getOrCreateType typeMap
                            let root = makeRootType(asm, ns, typeName, [
                                ProviderDsl.ChildType(
                                    makeCustomType(
                                        tsType.Namespace, 
                                        tsType.Files 
                                        |> List.map(fun f -> 
                                            ProviderDsl.ChildType(
                                                makeCustomType(
                                                    f.ModuleName,
                                                    f.Modules |> List.map(fun m -> 
                                                        if m.Name <> f.ModuleName && String.IsNullOrEmpty(m.Name) = false
                                                        then [
                                                                ProviderDsl.ChildType(
                                                                    makeCustomType(
                                                                        m.Name, 
                                                                        m.Types |> List.map (toType typeMap None),
                                                                        true
                                                                    )
                                                                )
                                                             ]
                                                        else m.Types |> List.map (toType typeMap None)
                                                    ) |> List.collect id
                                            , true))
                                    ), true
                                ))
                            ])
                            root
                    | _ -> failwith "unexpected parameter values"
                )
            )

        do this.AddNamespace(ns, [generator])

    [<assembly:TypeProviderAssembly>]
    do ()