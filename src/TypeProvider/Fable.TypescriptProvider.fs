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
   
    let runTs2FableModule packageName (dtsFile: string) out = 
        try 
            let p = new Process()
            let psi = new ProcessStartInfo()
            if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(Runtime.InteropServices.OSPlatform.Windows)
            then 
                psi.FileName <- "CMD.exe"
                psi.Arguments <- sprintf "/C %s \"%s\" \"%s\"" packageName dtsFile out
            else 
                psi.FileName <- "/bin/bash"
                psi.Arguments <- sprintf "%s \"%s\" \"%s\"" packageName dtsFile out
            psi.WindowStyle <- ProcessWindowStyle.Hidden
            p.StartInfo <- psi
            p.Start() |> ignore
            p.WaitForExit(10000)
        with 
        | ex -> failwith (sprintf "running ts2fable failed.Looking for %s at %s. Exception: %s" packageName packageName ex.Message)
    
    let rec determineFableVersion (dir: DirectoryInfo) =
        try 
            let toolsConfig = Path.Combine(dir.FullName, sprintf "./.config/dotnet-tools.json")
            let toolsConfigFileInfo = FileInfo(toolsConfig)
            if (isNull toolsConfigFileInfo) = false && toolsConfigFileInfo.Exists 
            then
                let configContent = Newtonsoft.Json.Linq.JObject.Parse(File.ReadAllText(toolsConfig))
                configContent.SelectToken("$.tools.fable.version").ToString()
            else determineFableVersion dir.Parent
        with 
        | ex -> failwith (sprintf "Could not determine Fable Version. Please make sure that fable is installed as a local dotnet tool. Message: %s" ex.Message)

    let loadMainDtsFile resolutionFolder moduleName = 
        // find package.json
        // let ts2FableScriptName = "ts2fable.js"
        // this is of course temporary
        let ts2FableJsonExportGlobalName = "ts2fable-json-export"
        
        let rec findPackageJson (dir: DirectoryInfo) moduleName =
            try 
                let packagePath = Path.Combine(dir.FullName, sprintf "./node_modules/%s/" moduleName)
                
                let packageJsonFile = FileInfo(Path.Combine(packagePath, "./package.json"))
                if (isNull packageJsonFile) = false && packageJsonFile.Exists then Ok(packageJsonFile.FullName, packagePath)
                else findPackageJson dir.Parent moduleName
            with 
            | ex -> Error(sprintf "Hint: Please check if the package is installed! ModuleName: %s. Dir: %s. ExMessage: %s. " moduleName dir.FullName ex.Message)

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
                if File.Exists(genFilePath) || (runTs2FableModule ts2FableJsonExportGlobalName dtsPath outPath)
                then 
                    let fableTypesJson = File.ReadAllText(genFilePath)
                    Decode.Auto.fromString<ts2fable.Syntax.FsFileOut>fableTypesJson
                else failwith "Could not load ts2fable Typing Information"
            | None -> failwith "Could not find index dts file"
    
    let normalizePropName = function 
                        | "``type``" -> "type", "type'"
                        | "type" -> "type", "type'"
                        | "``to``" -> "to", "to'"
                        | "to" -> "to", "to'"
                        | s -> s, s
                            

    let rec mapApiExport root path (selector:string) fableVersion (ex: ApiExport)=
        
            
        let rec mapErasedType selector (attachStatic :bool) (parent: ProvidedTypeDefinition) (erased: ErasedType) =
                match erased with 
                | ErasedType.Fn f ->
                    let returnType = mapErasedType selector false parent f.ReturnType
                    if attachStatic
                    then                    
                        let params' = f.Parameters |> List.map(fun (n,t) -> ProvidedParameter(n, (mapErasedType selector false parent t)))   
                        let method = ProviderDsl.makeImportMethod(f.Name, params', returnType, attachStatic, path, selector)
                        parent.AddMember method
                        parent :> Type  
                    else 
                       
                        let input = f.Parameters 
                                      |> List.map(fun (_,t) -> mapErasedType selector false parent t)
                                      |> fun t -> if t.Length = 0 
                                                  then 
                                                       typeof<unit>
                                                  elif t.Length = 1 
                                                  then 
                                                      t.[0]
                                                  else FSharp.Reflection.FSharpType.MakeTupleType(t |> List.toArray)

                        let t = FSharp.Reflection.FSharpType.MakeFunctionType(input,returnType)
                        t
                | ErasedType.ReactComponent rc -> 

                    let propFacade = ProvidedTypeDefinition(sprintf "I%sProps" rc.Name, Some(typeof<obj>), isInterface = true)
                    let listType = typedefof<list<_>>
                    let propTypeAsList = listType.MakeGenericType(propFacade)
                    parent.AddMember(propFacade)


                    let reactComponent = makeReactComponent(fableVersion, rc.Name, propTypeAsList, typeof<obj>, true, path, selector)
                    parent.AddMember(reactComponent)
                    
                    let (propertyBuilderType, props) = 
                                              match rc.Properties with 
                                              | Some(ErasedType.Custom c) -> ProvidedTypeDefinition(c.Name, Some(typeof<obj>)), c.Properties
                                              | Some _ -> failwith "Expected Custom type as property"
                                              | None -> ProvidedTypeDefinition(sprintf "%sProps" rc.Name, Some(typeof<obj>)), []
                    parent.AddMember(propertyBuilderType)

                    let propertyBuilderFns = props 
                                             |> List.map(fun (n,t) -> 
                                                    let (jsName, usageName) = normalizePropName n
                                                    ProvidedMethod(usageName, [ProvidedParameter("value", (mapErasedType selector false  propertyBuilderType t))], propFacade, false, makeTuple(jsName), true)
                                                )

                    propertyBuilderType.AddMembers(propertyBuilderFns)

                    propertyBuilderType :> Type
                | ErasedType.Custom c -> 
                    let t = ProvidedTypeDefinition(c.Name, Some typeof<obj>)
                    let props = c.Properties |> List.map(fun (n,c) -> ProvidedParameter(n, (mapErasedType selector false t c)))
                    let ctor = ProviderDsl.makeCtor(fableVersion, props)
                    t.AddMember ctor
                    parent.AddMember t
                    t :> Type
                | ErasedType.Union u ->
                    let possibleTypes = u |> List.map(fun t -> mapErasedType selector false parent t)
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
                | ErasedType.Obj -> typeof<obj>
                | ErasedType.Bool -> typeof<bool>
                | ErasedType.Int -> typeof<int>
                | ErasedType.Float -> typeof<float>
                | ErasedType.Array t -> (mapErasedType selector false parent t).MakeArrayType()
                | ErasedType.Option o -> typedefof<Option<obj>>.MakeGenericType(mapErasedType selector false parent o)
                | ErasedType.Unit -> typeof<unit>
                | _ -> typeof<string> //parent :> Type
                

        let types = match ex.Type' with 
                            | ErasedType.Custom c when c.Name.Contains("IExport") -> (c.Properties)
                            | c -> [ex.Name, c]
                            
        let selectors = selector.Split(',') |> Array.map(fun s -> s.Trim()) |> Array.toList
        types 
        |> List.choose(fun (n,t) -> 
            match selectors with 
            | "default"::[] -> Some("default", t)
            | "*"::[] -> Some("*", t)
            | s when s |> List.contains n -> Some(n, t)
            | _ -> None
        )
        |> List.iter(fun (selector, t) -> mapErasedType selector true root t |> ignore)
        
        

    [<TypeProvider>]
    type public TypescriptProvider (config : TypeProviderConfig) as this =
        inherit TypeProviderForNamespaces (config)
        let asm = System.Reflection.Assembly.GetExecutingAssembly()
        let ns = "Fable.TypescriptProvider"
        
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
                            let fableVersion = determineFableVersion (DirectoryInfo(config.ResolutionFolder))
                            let fableLibVer = sprintf "./.fable/fable-library.%s/Util.js" fableVersion
                            try 
                                let root = makeRootType(asm, ns, typeName, true)
                                api  |> Seq.iter (mapApiExport root path selector fableLibVer) 
                                root
                            with 
                            | ex -> failwith ex.Message
                    | _ -> failwith "unexpected parameter values"
                )
            )

        do this.AddNamespace(ns, [generator])

    [<assembly:TypeProviderAssembly>]
    do ()
