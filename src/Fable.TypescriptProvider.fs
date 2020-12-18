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
   
    open Thoth.Json.Net
    open ts2fable.Syntax
    open Transform
    
    type CachedTypeInformation = {
        BaseType: ProvidedTypeDefinition
    }

  
   
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
        
        
        do System.AppDomain.CurrentDomain.add_AssemblyResolve(ResolveEventHandler(fun _ args ->
            let expectedName = (System.Reflection.AssemblyName(args.Name)).Name + ".dll"
            let asmPath = 
                config.ReferencedAssemblies
                |> Seq.tryFind (fun asmPath -> IO.Path.GetFileName(asmPath) = expectedName)
            match asmPath with
            | Some f -> Reflection.Assembly.LoadFrom f
            | None -> null))

        let staticParams = [ProvidedStaticParameter("module",typeof<string>)]
        // TODO rename generator (borrowed from JsonProvider). 
        // Add Options to add additional "render" method into type for direct react component import (main motivation for this project)
        let generator = ProvidedTypeDefinition(asm, ns, "Import", Some typeof<obj>, isErased = true)

        do generator.DefineStaticParameters(
            parameters = staticParams,
            instantiationFunction = (fun typeName pVals ->
                    match pVals with
                    | [| :? string as arg|] ->
                        match loadMainDtsFile config.ResolutionFolder arg with
                        | Error err -> failwith err
                        | Ok tsFile -> 
                            let (allTypes, typeMap) = Transform.createSimplifiedTypeMap tsFile
                            
                            let (exVar, exInterface, exportProps) = Transform.getTypeSignature typeMap
                            let typesToCreate = Transform.mapToErasableType exportProps
                            let importPath = exVar.Export.Value.Path
                            let importSelector = exVar.Export.Value.Selector
                            let generativeType = ProviderDsl.makeTypeWithMembers("_"+arg, true, [])
                            //let staticMethodHolder = ProviderDsl.makeType("_", true)
                            let root = makeRootType(asm, ns, typeName, [])

                            
                            
                            
                            //let addProperties (attachingType: ProvidedTypeDefinition) (props: ErasedType list) =  
                            //    let rec onProp (prim: (string * Type) -> 'a) =
                            //        function 
                            //        | ErasedType.String s -> prim(s, typeof<string>)
                            //        | ErasedType.Float f -> prim(f, typeof<float>)
                            //        | ErasedType.Int i -> prim(i, typeof<int>)
                            //        | ErasedType.Bool b -> prim(b, typeof<bool>)
                            //        | ErasedType.Option er -> 
                            //            onProp (fun (n,t) -> prim(n,  typedefof<Option<obj>>.MakeGenericType(t))) er
                            //        | ErasedType.Fn(name, args, ret, ft) ->
                            //            let (_,ret) = match ret with 
                            //                      | ErasedType.Type(n, _) -> 
                            //                        match typeCache.Get(n) with 
                            //                        | None -> failwith "Type should already exist"
                            //                        | Some t -> n, t.BaseType :> Type
                            //                      | t -> onProp id t
                                        
                            //            match args with 
                            //        | _ -> failwith "dudumm"
                            //    let onComplexType (fn: (string * #Reflection.MemberInfo) -> unit) =
                            //        function 
                            //        | ErasedType.Fn(name, args, ret, ft) ->
                                        
                            //            ()
                            //            //let ret = match onPrimitive ret with 
                            //            //          | Some (_, ret) -> ret
                            //            //          | None -> match ret with 
                            //            //                    | ErasedType.Type(n,_) -> 
                            //            //                        match typeCache.Get(n) with 
                            //            //                        | Some n -> n.BaseType
                            //            //                        | None -> failwith (sprintf "Return Type is unknown: %A" n)
                            //            ()

                                         
                            //    props |> List.iter (onProp (fun (n,t) -> attachingType.AddMember(ProviderDsl.makeProperty(n, t, true))))
                            //    props |> List.iter (onComplexType (fun (n,t) -> attachingType.AddMember(ProviderDsl.makeProperty(n, t, true))))
                            //    ()

                            //for t in erasedTypes do 
                            //    match t with 
                            //    | ErasedType.Inherits(t, base') ->
                            //        match base' with 
                            //        | ErasedType.ReactComponent(props) ->
                            //            match props with 
                            //            | None -> failwith "We expect props (atm)"
                            //            | Some reactProps -> 
                            //                match reactProps with 
                            //                | ErasedType.Fn(name, args, ret, kind) ->
                            //                    match kind with 
                            //                    | FsFunctionKind.Constructor ->
                            //                        // we deal with a react component ctor
                            //                        match ret with 
                            //                        | ErasedType.Inherits(comp, ErasedType.ReactComponent rootReactProps) -> 
                            //                            match comp with 
                            //                            | ErasedType.Type(compName, args) ->
                                                            
                            //                                root.AddMember (ProviderDsl.makeImportReactMethod(compName, [], typeof<Fable.React.ReactElement>, true, importPath, n)) 
                            //                            | _ -> failwith "Component support is WIP"
                            //                        | _ -> failwith "WIP"
                            //                | _ -> failwith "we expect a type as props"
                            //    ()

                            let createExportRoot (types: ErasedType list) = 
                                
                                types 
                                |> List.map(fun t ->
                                    match t with 
                                    | ErasedType.Fn(f when f.
                                )


                            let rec mapErasedType (root: ProvidedTypeDefinition) = function
                                | ErasedType.Inherits(t,base') -> 
                                    match base' with 
                                    | ErasedType.ReactComponent(p) -> 
                                        match p with 
                                        | None -> failwith "Not yet supported"
                                        | Some p -> mapErasedType root p
                                    | _ -> failwith (sprintf "Inheritance with %A is not supported yet " t)
                                | ErasedType.Type(n,p) -> 
                                        let t = ProviderDsl.makeType(n, true)
                                        let props = p |> List.map (mapErasedType t)
                                                      |> List.map(fun (n,t) -> ProviderDsl.makeProperty(n,t, true))
                                        t.AddMembers(props)
                                        root.AddMember t
                                        (n, t:> Type)
                                | ErasedType.Fn(n,a,r, fk) -> 
                                        match fk with 
                                        | FsFunctionKind.Constructor ->
                                            match r with 
                                            | ErasedType.Inherits(t,base') -> 
                                                match base' with 
                                                | ErasedType.ReactComponent(p) -> 
                                                    match t with 
                                                    | ErasedType.Type(n,args) -> 
                                                        let args = match p with 
                                                                   | None -> args |> List.map (mapErasedType root)
                                                                   | Some p -> [p |> mapErasedType root]
                                                        let args = args |> List.map (fun (n,t) -> ProvidedParameter(n,t))
                                                        //if exInterface.Members |> List.map Transform.getFsTypeName |> List.exists(fun x -> x = n)
                                                        //then 
                                                        root.AddMember (ProviderDsl.makeImportReactMethod(n, args, typeof<obj>, true, importPath, n)) 
                                                        (n, root :> Type)
                                                    | _ -> failwith (sprintf "React component support is WIP for %A" t)
                                                | _ -> failwith (sprintf "Inheritance with %A is not supported yet " t)
                                        | _ ->
                                            let (_,ret) = mapErasedType root r
                                            let args = a |> List.map (mapErasedType root)
                                                         |> List.map (fun (n,t) -> ProvidedParameter(n,t))

                                            if exInterface.Members |> List.map Transform.getFsTypeName |> List.exists(fun x -> x = n)
                                            then root.AddMember (ProviderDsl.makeImportMethod(n, args, ret, true, importPath, importSelector))
                                            else root.AddMember (ProviderDsl.makeNoInvokeMethod(n, args, ret, false))
                                            (n, root :> Type)
                                | ErasedType.Option(s) -> 
                                    let (n, t) = mapErasedType root s
                                    (n, typedefof<Option<obj>>.MakeGenericType(t))
                                | ErasedType.String s -> s, typeof<string>
                                | ErasedType.Float f -> f, typeof<float>
                                | _ -> "NOT MATCHED", typeof<string>
                            
                            
                            

                            //erasedTypes |> List.iter (mapErasedType root >> ignore)
                            
                            //root.AddMember root
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