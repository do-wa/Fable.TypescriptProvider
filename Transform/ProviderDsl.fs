module rec ProviderDsl

open System.Reflection
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open System
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open System
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns


let makeImportProperty(name: string, type': System.Type, path: string) =
    ProvidedProperty(name, type',false, isStatic = true, getterCode = (fun args -> <@@ Fable.Core.JsInterop.import name path @@>))

let makeImportAllProperty(name: string, type': System.Type, path: string) =
    ProvidedProperty(name, type',false, isStatic = true, getterCode = (fun args -> <@@ Fable.Core.JsInterop.importAll path @@>))

let makeProperty(name: string, type': System.Type, isAbstract: bool) = 
    ProvidedProperty(name, type', isAbstract)

let makeTypeWithMembers (name: string, isInterface: bool, members: #MemberInfo list) =
    let t = ProvidedTypeDefinition(name, baseType = Some typeof<obj>, hideObjectMethods = true, IsErased = false, isInterface = isInterface)
    
    t.AddMembers(members)
    t

let makeImplementingType(name:string, type': System.Type) =
    ProvidedTypeDefinition(name, baseType = Some type', hideObjectMethods = true, IsErased = true, isInterface = false)

let makeType(name:string, isInterface:bool) =
    ProvidedTypeDefinition(name, baseType = Some typeof<obj>, hideObjectMethods = true, IsErased = true, isInterface = isInterface)


let makeJsNativeMethod (name: string, params': ProvidedParameter list, returnType: System.Type, isStatic: bool) =
    ProvidedMethod(
        name, 
        params',
        returnType,
        false,
        (fun args -> <@@ Fable.Core.Util.jsNative @@>),
        isStatic = isStatic
    )

let makeImportDefaultMethod(name: string, params': ProvidedParameter list, returnType: System.Type, isStatic: bool, path: string) =
    ProvidedMethod(
        name, 
        params',
        returnType,
        false,
        (fun args -> <@@ Fable.Core.JsInterop.importAll path @@>),
        isStatic = isStatic
    )


let inline boxTyped (expr: Expr) = if expr.Type.IsAssignableFrom(typeof<float>) then <@ box(%%expr:float) @>
                                   elif expr.Type.IsAssignableFrom(typeof<string>) then <@ box(%%expr:string) @>
                                   else failwith "Type not supported yet"

// this was previously completely inline                                   
let inline exprAsFnArgs (args: Expr list) = 
                    args
                    |> List.rev
                    |> List.map(fun arg -> boxTyped arg )
                    |> List.fold (fun state e -> <@ %e::%state @>) <@ [] @>

                    
let inline invokeFableFn (path: string, selector:string) = fun (args: Expr list) -> <@@ (Fable.Core.JsInterop.import selector path : Fable.Core.JsInterop.JsFunc).Invoke(%(exprAsFnArgs args) |> List.toArray) @@>
let inline invokeFableFn1Arg (path: string, selector: string) = fun (args: Expr list) -> <@@ (Fable.Core.JsInterop.import selector path : Fable.Core.JsInterop.JsFunc).Invoke(%%args.[0] :> obj) @@>
let makeImportReactMethod(name: string, params': ProvidedParameter list, returnType: System.Type, isStatic: bool, path: string, selector: string) =
    ProvidedMethod(
        name, 
        params',
        returnType,
        false,
        (fun args -> <@@ (Fable.Core.JsInterop.import "createElement" "react" : Fable.Core.JsInterop.JsFunc).Invoke(Fable.Core.JsInterop.import selector path, %%args.[0]) @@>),
        isStatic = isStatic
    )

let makeImportMethod(name: string, params': ProvidedParameter list, returnType: System.Type, isStatic: bool, path: string, selector: string) =
    if params'.Length = 1 
    then
        ProvidedMethod(
            name, 
            params',
            returnType,
            false,
            invokeFableFn1Arg(path, selector),
            isStatic = isStatic
        )
    else 
        ProvidedMethod(
            name, 
            params',
            returnType,
            false,
            invokeFableFn(path, selector),
            isStatic = isStatic
        )

let inline exprAsFnArgs2 (args: Expr list, paramNames: string list) = 
    args
    |> List.rev
    |> List.mapi(fun i arg -> paramNames.[i], boxTyped arg )
    |> List.fold (fun state (n,e) -> <@ (n,%e)::%state @>) <@ [] @>

let createObj objDesc =
    Fable.Core.JsInterop.createObj objDesc

let makeStaticMethod (pars: ProvidedParameter list) =
    let paramNames = pars |> List.map(fun n -> n.Name)
    //ProvidedConstructor(pars, (fun args -> <@@ createObj(%(exprAsFnArgs2(args, paramNames))) @@>))
    ProvidedMethod("Props", pars, typeof<seq<string * obj>>, false, (fun args -> <@@ (%exprAsFnArgs2(args, paramNames)) @@>),true)
    //ProvidedConstructor(pars, (fun args -> <@@ Fable.Core.JsInterop.importAll "left-pad" @@>))


let makeImportAllConstructor(path: string) = 
     ProvidedConstructor([], (fun args -> <@@ Fable.Core.JsInterop.importAll path @@>))

let makeNoInvokeMethod(name: string, params': ProvidedParameter list, returnType: System.Type, isStatic: bool) = 
    ProvidedMethod(
        name, 
        params',
        returnType,
        true,
        isStatic = isStatic
    )

let makeDumbMethod(name: string, params': ProvidedParameter list, returnType: System.Type, isStatic: bool) = 
    ProvidedMethod(
        name, 
        params',
        returnType,
        false,
        (fun args -> <@@ "The object data" :> obj @@>),
        isStatic = isStatic
    )

let makeRootType(assembly: Assembly, nameSpace: string, typeName: string, members: #MemberInfo list) =
    let root = ProvidedTypeDefinition(assembly, nameSpace, typeName, baseType = Some typeof<obj>, hideObjectMethods = true, IsErased = true)
    root.AddMembers members
    root
