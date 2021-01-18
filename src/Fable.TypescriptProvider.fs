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
    
    
    
    //let rec private mapToProvidedType (parentType: ProvidedTypeDefinition) (provideTo: string * Type -> unit) (mappedType: MappedType)  =
    //    match mappedType with
    //    | Primitive p ->
    //        match p.TypeName,p.IsOptional with 
    //        | "string", true -> 
    //            provideTo(p.Name, typedefof<Option<obj>>.MakeGenericType(typeof<string>))
    //        | "string", false -> 
    //            provideTo(p.Name, typeof<string>)
    //        | "int", true -> 
    //            provideTo(p.Name, typedefof<Option<obj>>.MakeGenericType(typeof<int>))
    //        | "int", false -> 
    //            provideTo(p.Name, typeof<int>)
    //        | "float", true -> 
    //            provideTo(p.Name, typedefof<Option<obj>>.MakeGenericType(typeof<float>))
    //        | "float", false -> 
    //            provideTo(p.Name, typeof<float>)
    //        | "bool", true -> 
    //            provideTo(p.Name, typedefof<Option<obj>>.MakeGenericType(typeof<bool>))
    //        | "bool", false ->
    //           provideTo(p.Name, typeof<bool>)
    //        | "unit", _ ->
    //           provideTo(p.Name, typeof<unit>)
    //        | _ , _ -> 
    //           provideTo(Guid.NewGuid().ToString().Replace("-",""), typeof<string>)
    //        parentType
    //    | Function f ->
    //        let fParams = ResizeArray<ProvidedParameter>()
    //        let mutable retType : Type = typeof<obj>
    //        let addToFn = fun (n,t) -> fParams.Add(ProvidedParameter(n,t))
    //        f.Ret |> (mapToProvidedType parentType (fun (n,t) -> retType <- t) >> ignore)
    //        let fn = ProviderDsl.makeNoInvokeMethod(Option.defaultValue "Create" f.Source.Name, (fParams.ToArray() |> Array.toList), retType, false)
    //        parentType.AddMember(fn)
            
    //        parentType
    //    | Property p ->
    //        let name = p.Source.Name
    //        mapToProvidedType parentType (fun (n,t) -> provideTo(name, t)) p.Mapped 
                
    //    | IFace i ->
    //        let newType = ProviderDsl.makeType(i.Source.Name, false)
    //        let addToType = fun (n,t) -> newType.AddMember(ProviderDsl.makeProperty(n,t,false))
    //        i.Props |> List.iter ((mapToProvidedType newType addToType) >> ignore )
    //        parentType.AddMember(newType :> Type)
    //        parentType
    //    | _ -> parentType
    
    
    //let (|ProvidedReactComponent|_|) (mappedType: MappedType) = 
    //    match mappedType with 
    //        | Generic g -> 
    //            match g.TypeParam with 
    //            | IFace comp -> 
    //                match comp.Inherits |> List.tryHead with
    //                | Some(Generic reactComponent) ->
    //                    match reactComponent.TypeParam with
    //                    | React m when m.Member = "Component" -> 
    //                        match reactComponent.TypeArgs.Head with
    //                        | IFace props -> Some (IFace props)
    //                        | _ -> None
    //                    | _ -> None
    //                | _ -> None
    //            | _ -> None
    //        | _ -> None
           
    
    //let toProvidedTypes (rootType: ProvidedTypeDefinition) exportProps = 
    //    // these are the entry points
    //    exportProps 
    //        |> List.iter(function
    //                | MappedType.Function f ->
    //                    let t = mapToProvidedType rootType (fun (n,t) -> rootType.AddMember (ProviderDsl.makeProperty(n,t,false)))
    //                    ()
    //                | MappedType.Property p ->   
    //                     let propertyName = p.Source.Name
    //                     // extract mapped type
    //                     match p.Mapped with 
    //                     | MappedType.IFace i ->
    //                        // its mapped to an type
    //                        // we analyse the type properties and have following scenarios
    //                        // 1. The type has an constructor so we look at the return type of the ctor to determine the actual type
                            
    //                        let (returnType, args) = 
    //                            match i.Source.HasConstructor with 
    //                            | true -> 
    //                                let ctor = i.Props.Head // TODO: make this right
    //                                match ctor with 
    //                                | MappedType.Function ctorFunction -> 
    //                                    match ctorFunction.Ret with 
    //                                    | ProvidedReactComponent(IFace m) ->   
    //                                        let propType = ProviderDsl.makeType(m.Source.Name, false)
    //                                        // TODO: Create FActory Fn to instantiate Props
    //                                        let r = ResizeArray<string * Type>()
    //                                        m.Props |> List.iter ((mapToProvidedType propType (fun (n,t) -> r.Add(n,t)) >> ignore))
                                            
    //                                        let props = seq {
    //                                                    for (n,t) in r do
    //                                                    propType.AddMember(ProviderDsl.makeProperty(n,t,false))
    //                                                    yield ProvidedParameter(n, t)
    //                                        }
                                           
                                            
    //                                        propType.AddMember(ProviderDsl.makeStaticMethod(props |> Seq.toList))
    //                                        propType, props
    //                                | _ -> failwith "Expected Ctor to be a function"
    //                            | _ -> failwith "Expected Ctor at the moment"
                        
    //                        rootType.AddMember returnType
    //                        rootType.AddMember(ProviderDsl.makeImportReactMethod(propertyName, [ProvidedParameter("props", returnType)], typeof<obj>, true, "react-awesome-button",propertyName))
    //                        ()
    //                | _ -> 
    //                    rootType.AddMember (ProviderDsl.makeNoInvokeMethod("NOT IMPLEMENTED", [], typeof<obj>, true))
    //                    ()
    //    )
    
   
    let rec toType<'T> optional = 
        if optional then typedefof<Option<obj>>.MakeGenericType(typeof<'T>)
        else typeof<'T>

    [<TypeProvider>]
    type public TypescriptProvider (config : TypeProviderConfig) as this =
        inherit TypeProviderForNamespaces (config)
        let asm = System.Reflection.Assembly.GetExecutingAssembly()
        let ns = "Fable.TypescriptTypeProvider"
        
        
        do System.AppDomain.CurrentDomain.add_AssemblyResolve(ResolveEventHandler(fun _ args ->
            let expectedName = (System.Reflection.AssemblyName(args.Name)).Name + ".dll"
            let asmPath = 
                config.ReferencedAssemblies
                |> Seq.tryFind (fun asmPath -> IO.Path.GetFileName(asmPath) = expectedName)
            match asmPath with
            | Some f -> Reflection.Assembly.LoadFrom f
            | None -> null))

        let staticParams = [ProvidedStaticParameter("selector",typeof<string>); ProvidedStaticParameter("path",typeof<string>)]
        // TODO rename generator (borrowed from JsonProvider). 
        // Add Options to add additional "render" method into type for direct react component import (main motivation for this project)
        let generator = ProvidedTypeDefinition(asm, ns, "Import", Some typeof<obj>, isErased = false)

        do generator.DefineStaticParameters(
            parameters = staticParams,
            instantiationFunction = (fun typeName pVals ->
                    match pVals with
                    | [| :? string as selector; :? string as path|] ->
                        match loadMainDtsFile config.ResolutionFolder path with
                        | Error err -> failwith err
                        | Ok tsFile -> 
                            let (allTypes, typeMap) = Transform.createSimplifiedTypeMap tsFile
                            
                            let (exVar, exInterface, exportProps) = Transform.getTypeSignature typeMap
                            let typesToCreate = Transform.mapToErasableType exportProps
                            let importPath = exVar.Export.Value.Path
                            let importSelector = exVar.Export.Value.Selector
                            //let staticMethodHolder = ProviderDsl.makeType("_", true)
                            let tempAsm = ProvidedAssembly()
                            let root = makeRootType(tempAsm, ns, typeName, false, [])
                            
                            //let generateType (root: ProvidedTypeDefinition) (type': MappedType) =
                            //    match type' with 
                            //    | Primitive p -> 
                            //         match p.TypeName with 
                            //         | "string" ->  p.Name, toType<string> p.IsOptional 
                            //         | "int" -> p.Name,  toType<int> p.IsOptional 
                            //         | "float" -> p.Name,  toType<float> p.IsOptional 
                            //         | "bool" -> p.Name,  toType<bool> p.IsOptional 
                            //         | _ -> p.Name, typeof<string>
                            //    | Union u -> 
                            //         u.Name, typeof<Fable.Core.U2<string,string>>
                            //    | _ -> "", typeof<string>

                            
                            //let generateMembers (root: ProvidedTypeDefinition) (members: MappedType list) = 
                            //    let rec generateMembers members (addList: ProvidedParameter list) = 
                            //        match members with 
                            //        | [] -> addList
                            //        | x::xs ->
                            //             let (name, t) = generateType root x
                            //             generateMembers xs (ProvidedParameter(name, t)::addList)
                            //    generateMembers members []

                 
                            //let generateInterface (root: ProvidedTypeDefinition) (mapped: MappedType list) = 
                            //    // root iface for exports vars
                            //    // we append the interface member to our root type
                            //    for t in mapped do
                            //     match t with 
                            //     | Function(f) ->
                            //         let args = generateMembers root (f.Args |> List.map snd)
                            //         let (name, ret) = generateType root f.Ret
                            //         root.AddMember(ProviderDsl.makeImportMethod(f.Name, args, ret, true, path, selector))
                            //         ()

                            ///generateInterface root exportProps
                            let iface2 = ProvidedTypeDefinition("leftPad", Some(typeof<obj>), isInterface = true, isErased = false)
                            iface2.AddMember(ProvidedProperty("prop1", typeof<string>, false, fun expr -> expr.[0]))
                            let iface = ProviderDsl.makeTypeWithMembers("IModule", true, [
                                //ProviderDsl.makeNoInvokeMethod("Test", [], typeof<obj>, true) :> System.Reflection.MemberInfo; 
                                ProviderDsl.makeProperty("bla", typeof<string>, true) :> System.Reflection.MemberInfo])
                  
                            root.AddMember iface2
                            root.AddMember(iface)
                            

                            //erasedTypes |> List.iter (mapErasedType root >> ignore)
                            
                            //root.AddMember root
                            
                            tempAsm.AddTypes([root])
                            root
                    | _ -> failwith "unexpected parameter values"
                )
            )

        do this.AddNamespace(ns, [generator])

    [<assembly:TypeProviderAssembly>]
    do ()
