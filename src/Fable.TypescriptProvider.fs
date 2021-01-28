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
    
    let rec mapApiExport asm ns (DEV_FABLE_LIB_VER) (path) (selector) (ex: ApiExport) =
        let rec mapErasedType (isRoot :bool) (parent: ProvidedTypeDefinition) (erased: ErasedType) =
                match erased with 
                | ErasedType.Fn f ->
                    let returnType = mapErasedType false parent f.ReturnType
                    let params' = f.Parameters |> List.map(fun (n,t) -> ProvidedParameter(n, (mapErasedType false parent t)))
                    if isRoot then 
                        let method = ProviderDsl.makeImportMethod(f.Name, params', returnType, isRoot, path, selector)
                        parent.AddMember method
                        parent :> Type
                    else failwith "Figure other function out"
                | ErasedType.Custom c -> 
                    let t = ProvidedTypeDefinition(c.Name, Some typeof<obj>)
                    let props = c.Properties |> List.map(fun (n,c) -> ProvidedParameter(n, (mapErasedType false t c)))
                    let ctor = ProviderDsl.makeCtor(DEV_FABLE_LIB_VER, props)
                    t.AddMember ctor
                    parent.AddMember t
                    parent :> Type
                | ErasedType.Union u ->
                    let possibleTypes = u |> List.map(fun t -> mapErasedType false parent t)
                    match possibleTypes.Length with 
                    | 1 -> possibleTypes.[0]
                    | o -> 
                        match o with 
                        | 2 -> 
                            let tdo = typedefof<U2<_,_>>
                            let u2 = tdo.MakeGenericType(possibleTypes |> List.toArray)
                            u2
                        | _ -> failwith "Implement more than 2 cases"
                | ErasedType.Enum(enum) -> 
                    let enumBaseType = ProvidedTypeDefinition(enum.Name, None)
                    
                    match enum.Type with 
                    | FsEnumCaseType.Numeric ->
                        enumBaseType.SetEnumUnderlyingType(typeof<float>)
                        enum.Cases 
                        |> List.iteri(fun i case -> 
                            enumBaseType.AddMember(ProvidedField.Literal(case.Name, enumBaseType, Convert.ToDouble (Option.defaultValue (i.ToString()) case.Value)))
                        )
                    | FsEnumCaseType.String
                    | FsEnumCaseType.Unknown ->
                        enumBaseType.SetEnumUnderlyingType(typeof<string>)
                        enum.Cases 
                        |> List.iteri(fun i case -> enumBaseType.AddMember(ProvidedField.Literal(case.Name, enumBaseType, Option.defaultValue (case.Name+i.ToString()) case.Value)))
                    
                    enumBaseType :> Type
                | ErasedType.String -> typeof<string>
                | ErasedType.Bool -> typeof<bool>
                | ErasedType.Int -> typeof<int>
                | ErasedType.Float -> typeof<float>
                | ErasedType.Array t -> (mapErasedType false parent t).MakeArrayType()
                | ErasedType.Option o -> typedefof<Option<obj>>.MakeGenericType(mapErasedType false parent o)
                | _ -> parent :> Type
                

        let (name, types) = match ex.Type' with 
                            | ErasedType.Custom c when c.Name.Contains("IExport") -> ex.Name, (c.Properties |> List.map snd)
                            | c -> ex.Name, [c]

        let root = makeRootType(asm, ns, name, true)
        
        let members = types |> List.map (mapErasedType true root)
                           
        root.AddMembers members 
        root

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

        let staticParams = [
                ProvidedStaticParameter("selector",typeof<string>)
                ProvidedStaticParameter("path",typeof<string>)
            ]
        let generator = ProvidedTypeDefinition(asm, ns, "Import", Some typeof<obj>, isErased = true)

        do generator.DefineStaticParameters(
            parameters = staticParams,
            instantiationFunction = (fun typeName pVals ->
                    match pVals with
                    | [| :? string as selector; :? string as path |] ->
                        match loadMainDtsFile config.ResolutionFolder path with
                        | Error err -> failwith err
                        | Ok tsFile -> 
                            let (_, typeMap) = Transform.createSimplifiedTypeMap tsFile
                            let api = Transform.buildApi typeMap

                            
                            let root = makeRootType(asm, ns, typeName, true)
                            
                            root
                            //try 
                                
                            //    let apiExports = api |> Seq.map (mapApiExport asm ns "./.fable/fable-library.3.1.1/Util.js" path selector) |> Seq.head // single export definition only atm
                            //    apiExports
                            //with 
                            //| ex -> failwith ex.Message
                    | _ -> failwith "unexpected parameter values"
                )
            )

        do this.AddNamespace(ns, [generator])

    [<assembly:TypeProviderAssembly>]
    do ()
