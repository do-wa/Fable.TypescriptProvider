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
    
    type CachedTypeInformation = {
        BaseType: ProvidedTypeDefinition
        Methods: ProvidedMethod list
        Properties: ProvidedProperty list
    }

    // TODO: make this dict method all simpler
 
    let getAndCacheType (forceValueUpdate :bool) (types: Collections.Generic.Dictionary<string list, CachedTypeInformation>) (name: string list) (typeFactory: string -> ProvidedTypeDefinition)  =
        if forceValueUpdate = true 
        then 
            let type' = typeFactory (name |> List.last)
            types.[name] <- { BaseType = type'; Methods = []; Properties = [] }

        match types.TryGetValue name with
        | false, _ -> 
            let type' = typeFactory (name |> List.last)
            types.[name] <-  { BaseType = type'; Methods = []; Properties = [] }
            types.[name].BaseType
        | true, type' -> type'.BaseType
    let getOrCreateType = getAndCacheType false
    let rec getProvidedType  (types: Collections.Generic.Dictionary<string list, CachedTypeInformation>) (name: string list) = 
        match types.TryGetValue name with
        | false, _ when name.Length > 1 -> getProvidedType types name.Tail
        | false, _ -> None
        | true, type' -> Some type'
    let updateProvidedType (types: Collections.Generic.Dictionary<string list, CachedTypeInformation>) (name: string list) (method: ProvidedMethod option) (prop: ProvidedProperty option) =
        match types.TryGetValue name with
        | false, _ -> failwith "Type does not exist"
        | true, type' -> 
            match method with 
            | Some m -> types.[name] <- { type' with Methods = m::type'.Methods }
            | None -> ()
            match prop with 
            | Some p -> types.[name] <- { type' with Properties = p::type'.Properties }
            | None -> ()

    let mapType (types: Collections.Generic.Dictionary<string list, CachedTypeInformation>) (typeName:string list) (isOption: bool) =
        let customType = getProvidedType types typeName
        let erasedType = 
            match customType with
            | Some t -> ErasedType.Custom(t.BaseType)
            | None -> 
            match typeName with 
                      | _::["string"] when not isOption -> ErasedType.String
                      | _::["string"] when isOption -> ErasedType.Option(ErasedType.String)
                      | _::["int"] when not isOption -> ErasedType.Int
                      | _::["int"] when isOption -> ErasedType.Option(ErasedType.Int)
                      | _::["float"] when not isOption -> ErasedType.Float
                      | _::["float"] when isOption -> ErasedType.Option(ErasedType.Float)
                      | _::["bool"] when not isOption -> ErasedType.Bool
                      | _::["bool"] when isOption -> ErasedType.Option(ErasedType.Bool)
                      | _ -> ErasedType.String // Todo add Array and others
        erasedType

    let rec toType (types: Collections.Generic.Dictionary<string list, CachedTypeInformation>) (moduleName: string) (parentType: ProvidedTypeDefinition option)  = function 
        | ts2fable.Syntax.FsType.Enum enum -> 
            let enumBaseType = 
                match parentType with
                | None -> getOrCreateType types [moduleName;enum.Name] (fun name -> ProvidedTypeDefinition(name, None)) 
                | Some t -> getAndCacheType true types [moduleName;t.Name] (fun name -> ProvidedTypeDefinition(name, None)) 
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
            let interfaceType = getOrCreateType types [moduleName;interface'.Name] (fun n -> ProviderDsl.makeType(n, true)) 
            interface'.Members
            |> List.collect (toType types moduleName (Some interfaceType))
        | ts2fable.Syntax.FsType.Property prop when parentType <> None -> 
            match prop.Type with 
            | ts2fable.Syntax.FsType.Mapped mapped -> 
                let mappedType = mapType types [moduleName;mapped.Name] prop.Option
                let property = ProviderDsl.makeProperty(prop.Name, mapErasedType mappedType, true)
                updateProvidedType types [moduleName; parentType.Value.Name] None (Some property) 
                parentType.Value.AddMember(property)
                [parentType.Value]
            | _ ->  [parentType.Value]
        | ts2fable.Syntax.FsType.Function func -> 
            match parentType with 
            | None -> failwith "Not supported" 
            | Some parentType -> 
                let params' = 
                    func.Params 
                    |> List.map(fun param -> 
                        let name = param.Name
                        match param.Type with 
                        | ts2fable.Syntax.FsType.Union union ->
                                   let possibleTypes = union.Types 
                                                       |> List.map (function | FsType.Mapped m -> mapType types [moduleName;m.Name] union.Option | _ -> failwith "Non-Mapped Type not supported yet")
                                   match possibleTypes.Length with 
                                   | 1 -> name, possibleTypes.[0]
                                   | 2 -> let tdo = typedefof<U2<_,_>>
                                          let u2 = tdo.MakeGenericType(mapErasedType possibleTypes.[0],mapErasedType possibleTypes.[1]) // unpack
                                          name, ErasedType.Custom(u2) // pack again
                                   | _ -> failwith "Currently only supporting Unions up to 2 values"
                                    
                        | ts2fable.Syntax.FsType.Mapped mapped -> 
                            let mappedType = mapType types [moduleName;mapped.Name] param.Optional
                            name, mappedType
                        | _ ->  ("PARAM_"+(System.Guid.NewGuid().ToString().Replace("-",""))), ErasedType.String // TODO: support other types
                    ) 
                    |> List.map(fun (name, type') -> ProvidedParameter(name, mapErasedType type'))
                
                
                let method = ProviderDsl.makeNoInvokeMethod(Option.defaultValue "Invoke" func.Name, params', mapErasedType ErasedType.String, false) // TODO: support return type
                updateProvidedType types [moduleName; parentType.Name] (Some method) None 
                parentType.AddMember(method)
                [parentType]

        | _ -> [ProviderDsl.makeType(System.Guid.NewGuid().ToString().Replace("-",""), true)] // TODO: unions, tuples, generics... what else?
    
    let debugPrintDict (dict: System.Collections.Generic.Dictionary<string list, CachedTypeInformation>) =
        let lines = seq {
            for kvp in dict do
                yield sprintf "%s: %s" (kvp.Key |> String.concat ".") (kvp.Value.BaseType.Name)
        }
        lines |> String.concat "\n"

    let rec toModule (path: string, selector: string) (typeMap: Collections.Generic.Dictionary<string list, CachedTypeInformation>) (module': FsModule)  =
        let nestedModules = 
                   module'.Types 
                   |> List.tryPick(function | FsType.Module m -> Some m | _ -> None)

        let moduleDeclaration = 
            match nestedModules 
                  |> Option.bind (fun fsm -> Some (getAndCacheType false typeMap [fsm.Name] (fun _ -> ProviderDsl.makeType(fsm.Name, false)))) with 
            | Some md -> md
            | None -> // bind to parent 
                      match getProvidedType typeMap [(module'.Name)] with 
                      | Some md -> md.BaseType
                      | None -> failwith "We have no module for binding"

        let innerModuleTypes = 
            match nestedModules with 
            | None -> None
            | Some fsm -> fsm.Types
                          |> List.rev
                          |> List.collect (toType typeMap (moduleDeclaration.Name) (Some moduleDeclaration))
                          |> Some

        let interfaces =
            module'.Types 
            |> List.choose(function | FsType.Module _ | FsType.Variable _ -> None | other -> Some other)
            |> List.rev
            |> List.collect (toType typeMap (moduleDeclaration.Name) (Some moduleDeclaration))

        let variables = 
            module'.Types
            |> List.choose(function | FsType.Variable v -> Some v | _ -> None)
            |> List.rev
            |> List.choose(fun var -> 
                
                //let prop = ProviderDsl.makeProperty(var.Name)

                match var.Type with 
                | ts2fable.Syntax.FsType.Mapped mapped -> 
                    let typeName = mapped.Name.Split('.') |> Array.toList
                    match getProvidedType typeMap typeName with
                    | None -> failwith (sprintf "Type %s should exist. Current Type Map: %s" (typeName |> String.concat ".") (debugPrintDict typeMap))
                    | Some t -> 
                        let (methodImporter, propImporter, export) = 
                            match var.Export with 
                            | Some ex -> ProviderDsl.makeImportAllMethod,ProviderDsl.makeImportAllProperty, ex
                            | _ -> failwith "Module Export expected"
                        let methods = 
                            t.Methods 
                            |> List.map(fun m -> methodImporter(m.Name, m.Parameters |> Array.toList, m.ReturnType, true, path, selector) :> Reflection.MemberInfo)
                        let properties = 
                            t.Properties
                            |> List.map(fun p -> propImporter(p.Name, p.PropertyType, export.Path) :> Reflection.MemberInfo)
                        Some (methods @ properties)
                        //match var.Export with 
                        //| Some ex -> 
                        //    match ex.Selector with 
                        //    | "*" -> Some([ProviderDsl.makeImportAllProperty(var.Name, t.BaseType, ex.Path)])
                        //    | _ -> failwith "Module Export not supported yet"
                        //| None -> failwith "Variable without export not supported yet"

                        //let method = t.Methods |> List.tryFind(fun m -> m.Name = var.Name)     
                        //match method with 
                        //| None -> 
                        //    let props = t.Properties |> List.tryFind(fun m -> m.Name = var.Name)
                        //    match props with 
                        //    | None ->  failwith (sprintf "Could not find Property %s in Type %s variable declaration. This should be part of . Current Type Map: %s" (var.Name) (t.BaseType.Name) (debugPrintDict typeMap))
                        //    | Some p -> 
                        //        let property = ProviderDsl.makeProperty(p.Name, p.PropertyType, false) :> System.Reflection.MemberInfo
                        //        Some [property]
                        //| Some m -> 
                        //        match var.Export with 
                        //        | None -> None
                        //        | Some ex -> 
                        //            match ex.Selector with 
                        //            | "*" -> let method = ProviderDsl.makeImportAllMethod(m.Name, m.Parameters |> Array.toList, m.ReturnType, true, ex.Path) :> System.Reflection.MemberInfo
                        //                     Some([
                        //                            //ProviderDsl.makeImportAllProperty(var.Name, t.BaseType, ex.Path)
                        //                            method
                        //                         ])
                        //            | _ -> failwith "Not supported yet"
                    //match getProvidedType typeMap typeName with
                    //| None -> failwith "Type should exist"
                    //| Some t -> 
                    //    //match var.Export with 
                    //    //| Some ex -> 
                    //    //    match ex.Selector with 
                    //    //    | "*" -> Some(ProviderDsl.makeImportAllProperty(var.Name, t.BaseType, ex.Path))
                    //    //    | _ -> failwith "Module Export not supported yet"
                    //    //| None -> failwith "Variable without export not supported yet"
                    //    let method = t.Methods |> List.tryFind(fun m -> m.Name = var.Name)     
                    //    match method with 
                    //    | None -> 
                    //        let props = t.Properties |> List.tryFind(fun m -> m.Name = var.Name)
                    //        match props with 
                    //        | None -> failwith "Could not find matching module variable declaration"
                    //        | Some p -> 
                    //            let property = Some( ProviderDsl.makeProperty(p.Name, p.PropertyType) :> System.Reflection.MemberInfo)
                    //            property
                    //    | Some m -> 
                    //            match var.Export with 
                    //            | None -> None
                    //            | Some ex -> 
                    //                match ex.Selector with 
                    //                | "*" -> let method = Some(ProviderDsl.makeImportMethod(m.Name, m.Parameters |> Array.toList, m.ReturnType, true, ex.Path) :> System.Reflection.MemberInfo)
                    //                         method
                    //                | _ -> failwith "Not supported yet"
                           
                    
                | _ -> failwith "Not supported.. yet"
            )
            |> List.collect id
        //moduleDeclaration.AddMembers nestedTypes
        if innerModuleTypes <> None then moduleDeclaration.AddMembers(innerModuleTypes.Value)
        moduleDeclaration.AddMembers interfaces
        moduleDeclaration.AddMembers variables

        // testarea
        let otherT = ProvidedTypeDefinition("Nested", Some typeof<obj>)
        let ctorT = ProvidedConstructor([], fun args -> 
                        let qargs = args |> List.map(fun arg -> Expr.Coerce(arg, typeof<obj>)) |> (fun nargs -> Expr.NewArray(typeof<obj>, nargs)) 
                        <@@ (Fable.Core.JsInterop.import "createObj" "./.fable/fable-library.3.1.1/Util.js" : Fable.Core.JsInterop.JsFunc).Invoke([| "A", box "B" |]) @@>
                        )
        otherT.AddMember(ctorT)
        let t = ProvidedTypeDefinition("Props", Some typeof<obj>)
        let props = [
                ProvidedParameter("A", typeof<string>)
                ProvidedParameter("B", typeof<int>)
                ProvidedParameter("C", otherT)
            ]

        let ctor = ProvidedConstructor(props, 
            invokeFableObjFn("./.fable/fable-library.3.1.1/Util.js", ["A";"B";"C"])
        )
        t.AddMember(ctor)
        // end the tests!
        moduleDeclaration.AddMember otherT
        moduleDeclaration.AddMember t
        moduleDeclaration
    // stringvaraint         
    //let extractTs2Fable scriptName copyToPath = 
    //    try 
    //        let assembly = System.Reflection.Assembly.GetExecutingAssembly();
    //        let resourceNames = assembly.GetManifestResourceNames()
    //        if (isNull resourceNames) then failwith "TypeProvider Resources could not be loaded...this is a bug"
    //        let resourceName  = resourceNames |> Array.tryFind(fun n -> n.EndsWith(scriptName))
    //        match resourceName with 
    //        | None -> failwith "Typeprovider ts2fable Resource could not be found.. this is a bug"
    //        | Some resourceName -> 
    //            use stream = assembly.GetManifestResourceStream(resourceName)
    //            use reader = new StreamReader(stream)
    //            let fc = reader.ReadToEnd()
    //            File.WriteAllText(copyToPath, fc)
    //    with 
    //    | ex -> failwith (sprintf "extractTs2Fable failed. %s" ex.Message)
    
    // TODO: Support multi plattform, theres probably is a good .net package for this
    let runTs2FableModule scriptPath scriptName (dtsFile: string) out = 
        try 
            let p = new Process()
            let psi = new ProcessStartInfo()
            psi.FileName <- "CMD.exe"
            psi.WorkingDirectory <- scriptPath
            psi.Arguments <- sprintf "/C node %s \"%s\" \"%s\"" scriptName dtsFile out
            psi.WindowStyle <- ProcessWindowStyle.Hidden
            p.StartInfo <- psi
            p.Start() |> ignore
            p.WaitForExit(10000)
        with 
        | ex -> failwith (sprintf "running ts2fable failed.Looking for %s at %s. Exception: %s" scriptName scriptPath ex.Message)

    let loadMainDtsFile resolutionFolder moduleName = 
        // find package.json
        let ts2FableScriptName = "ts2fable.js"
        // this is of course temporary
        let ts2FableFolderPath = "C:\\Users\\dominik\\Projects\\Fable.JsonProvider-master\\Fable.JsonProvider-master\\src\\ts2fable-master\\build\\" //"Path.GetTempPath()
        
        let rec findPackageJson (dir: DirectoryInfo) moduleName =
            try 
                let packagePath = Path.Combine(dir.FullName, sprintf "./node_modules/%s/" moduleName)
                let packageJsonFile = FileInfo(Path.Combine(packagePath, "./package.json"))
                if (isNull packageJsonFile) = false && packageJsonFile.Exists then Ok(packageJsonFile.FullName, packagePath)
                else findPackageJson dir.Parent moduleName
            with 
            | ex -> Error(sprintf "ExMessage: %s. ModuleName: %s. Dir: %s" ex.Message moduleName dir.FullName)

        let packageJsonFile = findPackageJson (DirectoryInfo(resolutionFolder)) moduleName
        match packageJsonFile with 
        | Error msg -> failwith (sprintf "Could not locate package.json. %s" msg)
        | Ok(packageJsonFile, packagePath) -> 
            // this is the happy path. Typings should be declared in the package.json <-- DONE
            // fallback 1: Find any index.dts in the root folder of the project       <-- DONE
            // otherwise the user should link/path to the right file                  <-- TODO
            let packageJsonContent = Newtonsoft.Json.Linq.JObject.Parse(File.ReadAllText(packageJsonFile))
            let typingsInfo = [packageJsonContent.SelectToken("$.types"); packageJsonContent.SelectToken("$.typings")]
            let versionInfo = packageJsonContent.SelectToken("$.version").ToString()
            let dtsMainFilePath = 
                match typingsInfo |> List.tryPick(fun ti -> if (isNull ti) = false then Some(ti.ToString()) else None) with
                | Some file -> Some file
                | None when File.Exists(Path.Combine(packagePath,"./index.d.ts")) -> Some(Path.Combine(packagePath,"./index.d.ts"))
                | None -> None
                               
            match dtsMainFilePath with 
            | Some dtsMainPath -> 
                let dtsPath = Path.Combine(packagePath, dtsMainPath)
                let outPath = Path.Combine(Path.GetTempPath(), (sprintf "%s.%s.fs" moduleName versionInfo))
                let genFilePath = sprintf "%s.json" outPath
                // check if file already exists if not run module
                if File.Exists(genFilePath) || (runTs2FableModule ts2FableFolderPath ts2FableScriptName dtsPath outPath)
                then 
                    let fableTypesJson = File.ReadAllText(genFilePath)
                    Decode.Auto.fromString<ts2fable.Syntax.FsFileOut>fableTypesJson
                else failwith "Could not load ts2fable Typing Information"
            | None -> failwith "Could not find index dts file"
                

    [<TypeProvider>]
    type public TypescriptProvider (config : TypeProviderConfig) as this =
        inherit TypeProviderForNamespaces (config)
        let asm = System.Reflection.Assembly.GetExecutingAssembly()
        let ns = "Fable.TypescriptProvider"
        
        let staticParams = [
                ProvidedStaticParameter("selector", typeof<string>)
                ProvidedStaticParameter("path",typeof<string>)
            ]
        // TODO rename generator (borrowed from JsonProvider). 
        // Add Options to add additional "render" method into type for direct react component import (main motivation for this project)
        let generator = ProvidedTypeDefinition(asm, ns, "Import", Some typeof<obj>, isErased = true)

        do generator.DefineStaticParameters(
            parameters = staticParams,
            instantiationFunction = (fun typeName pVals ->
                    match pVals with
                    | [|:? string as selector ; :? string as path|] ->
                        match loadMainDtsFile config.ResolutionFolder path with
                        | Error err -> failwith err
                        | Ok tsType -> 
                            let typeMap = Collections.Generic.Dictionary<string list, CachedTypeInformation>()
                            let files = tsType.Files

                            let otherFiles = files |> List.choose(fun f -> match f.Kind with | FsFileKind.Extra _ -> Some f | _ -> None)
                            let indexFile = files |> List.pick(fun f -> match f.Kind with | FsFileKind.Index -> Some f | _ -> None) // TODO: more than one?

                            let otherModules = otherFiles|> List.collect(fun m -> m.Modules |> List.map (toModule (path, selector) typeMap))
                            let indexModules = indexFile.Modules |> List.map (toModule (path, selector) typeMap) 

                            let allModules = indexModules @ otherModules

                            let root = makeRootType(asm, ns, typeName, allModules)
                            
                            
                            root
                    | _ -> failwith "unexpected parameter values"
                )
            )

        do this.AddNamespace(ns, [generator])

    [<assembly:TypeProviderAssembly>]
    do ()


//module FileProvider =
//    open System.IO
//    open System.Text
//    open Samples.FSharp.ProvidedTypes
//    open Microsoft.FSharp.Core.CompilerServices

//    [<TypeProvider>]
//    type FilePr() as this =
//        inherit TypeProviderForNamespaces()
//        let asm,ns = System.Reflection.Assembly.GetExecutingAssembly(),"FileProvider"
//        let IniTy = ProvidedTypeDefinition(asm, ns, "FileProv", None)
//        do IniTy.DefineStaticParameters([ProvidedStaticParameter("path", typeof<string>)],
//                                        fun tyName [|:? string as path|] ->
//                                             let ty = ProvidedTypeDefinition(asm, ns, tyName, None)
//                                             [ProvidedConstructor([ProvidedParameter("path",typeof<string>)],
//                                                                  InvokeCode=(fun [path]-> <@@ %%path:string @@>))
//                                              ProvidedConstructor([],InvokeCode=(fun _ -> <@@ Directory.GetCurrentDirectory() @@>))]
//                                               |>ty.AddMembers 
//                                             Directory.GetFiles(path)|>Seq.map (fun name->FileInfo(name).Name)
//                                              |>Seq.iter (fun (name)->
//                                                           let sty=ProvidedTypeDefinition(name,None)
//                                                           ty.AddMember sty
//                                                           [ProvidedProperty("Text",typeof<string>,
//                                                                             GetterCode=fun [path] -> <@@ Path.Combine((%%path:obj):?>string,name)|>File.ReadAllText @@>)
//                                                            ProvidedProperty("StreamR",typeof<Stream>,
//                                                                             GetterCode=fun [path] -> <@@ Path.Combine((%%path:obj):?>string,name)|>File.OpenRead @@>)
//                                                            ProvidedProperty("StreamW",typeof<Stream>,
//                                                                             GetterCode=fun [path] -> <@@ Path.Combine((%%path:obj):?>string,name)|>File.OpenWrite @@>)
//                                                            ProvidedProperty("Name",typeof<string>,
//                                                                             GetterCode=fun _ -> <@@ name @@>)
//                                                            ProvidedProperty("FullName",typeof<string>,
//                                                                             GetterCode=fun [path] -> <@@ Path.Combine((%%path:obj):?>string,name) @@>)]
//                                                             |>sty.AddMembers
//                                                           ProvidedMethod("GetText",[ProvidedParameter("Encode",typeof<Encoding>)],typeof<string>,
//                                                                          InvokeCode=fun [path;enc] -> <@@ File.ReadAllText( Path.Combine((%%path:obj):?>string,name),(%%enc:>Encoding)) @@>)
//                                                             |>sty.AddMember
//                                                           let prop=ProvidedProperty(name,sty,GetterCode=fun [arg] -> <@@ (%%arg:obj):?>string @@>)
//                                                           ty.AddMember prop)
//                                             ty)
//           this.AddNamespace(ns, [IniTy])
//    [<TypeProviderAssembly>]
//    do()