module rec Transform

open ts2fable
open ts2fable.Syntax
open System
open System.Collections.Generic

// TODO: Interop between ts2fable and TypeProvider 
// The ts2fable package is adjusted to return .json files with the type information instead of the .fs file
let sample = """{"Namespace":"index","Opens":["System","Fable.Core","Fable.Core.JS"],"Files":[{"Kind":"Index","FileName":"C:/Users/dominik/Projects/FSharp.Fable.TypescriptProvider/sample/Simple/node_modules/left-pad/index.d.ts","ModuleName":"left-pad","Modules":[{"HasDeclare":false,"IsNamespace":false,"Name":"","Types":[["Variable",{"Export":{"IsGlobal":false,"Selector":"*","Path":"left-pad"},"HasDeclare":true,"Name":"leftPad","Type":["Mapped",{"Name":"LeftPad.IExports","FullName":"LeftPad.IExports"}],"IsConst":true,"IsStatic":false,"Accessibility":null}],["Interface",{"Comments":[],"IsStatic":false,"IsClass":false,"Name":"IExports","FullName":"IExports","TypeParameters":[],"Inherits":[],"Members":[["Function",{"Comments":[],"Kind":"Regular","IsStatic":false,"Name":"leftPad","TypeParameters":[],"Params":[{"Comment":null,"Name":"str","Optional":false,"ParamArray":false,"Type":["Union",{"Option":false,"Types":[["Mapped",{"Name":"string","FullName":"string"}],["Mapped",{"Name":"float","FullName":"float"}]]}]},{"Comment":null,"Name":"len","Optional":false,"ParamArray":false,"Type":["Mapped",{"Name":"float","FullName":"float"}]},{"Comment":null,"Name":"ch","Optional":true,"ParamArray":false,"Type":["Union",{"Option":false,"Types":[["Mapped",{"Name":"string","FullName":"string"}],["Mapped",{"Name":"float","FullName":"float"}]]}]}],"ReturnType":["Mapped",{"Name":"string","FullName":"string"}],"Accessibility":null}]],"Accessibility":null}],["Module",{"HasDeclare":true,"IsNamespace":true,"Name":"LeftPad","Types":[],"HelperLines":[],"Attributes":[]}],["ExportAssignment","leftPad"]],"HelperLines":[],"Attributes":[]}]}],"AbbrevTypes":[]}"""

type Registered = {
    Parent: Registered option
    Actual: FsType
    Children: Registered list
} 

type Registration() =
     let dict = Dictionary<string, Registered>()
     let tryFind name =
        match dict.TryGetValue name with 
        | false, _ -> None
        | true, v -> Some v
     let addChildren name children = 
        let children = children @ dict.[name].Children
        dict.[name] <- { dict.[name] with Children = children }
     let addOrUpdate name addValue updater = 
        match dict.TryGetValue name with 
        | false, _ -> 
            dict.[name] <- addValue
            dict.[name]
        | true, v ->
            dict.[name] <- updater v
            dict.[name]

     member this.AddType(name: string, type' : FsType, directParent: string) =
        let parent = tryFind directParent
        let add = { Actual = type'; Parent = parent; Children = [] }
        if parent <> None then addChildren directParent [add] 
        addOrUpdate name add id



let rec toModule (registrat: Registration) (module': FsModule) =
    let nestedModules = 
               module'.Types 
               |> List.tryPick(function | FsType.Module m -> Some m | _ -> None)

    let moduleDeclaration = 
        match nestedModules 
              |> Option.bind (fun fsm -> Some (registrat.AddType(fsm.Name,fsm,module'.Name))) with 
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

    //let variables = 
    //    module'.Types
    //    |> List.choose(function | FsType.Variable v -> Some v | _ -> None)
    //    |> List.rev
    //    |> List.choose(fun var -> 
            
    //moduleDeclaration.AddMembers nestedTypes
    if innerModuleTypes <> None then moduleDeclaration.AddMembers(innerModuleTypes.Value)
    moduleDeclaration.AddMembers interfaces
    moduleDeclaration.AddMembers variables
    moduleDeclaration