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
open Transform



let makeRootType(assembly: Assembly, nameSpace: string, typeName: string, isErased : bool) =
    let root = ProvidedTypeDefinition(assembly, nameSpace, typeName, baseType = Some typeof<obj>, hideObjectMethods = true, IsErased = isErased)
    root

let inline invokeFableObjFn (libVersion:string, paramNames: string list) = fun (args: Expr list) -> 
    let qargs = args |> List.map(fun arg -> Expr.Coerce(arg, typeof<obj>)) |> (fun nargs -> Expr.NewArray(typeof<obj>, nargs))             
    <@@ 
        (Fable.Core.JsInterop.import "createObj" libVersion : Fable.Core.JsInterop.JsFunc).Invoke(%%qargs |> Array.toList |> List.mapi(fun i v -> box (paramNames.[i], v)))
    @@>
let inline invokeFableFn (path: string, selector:string) = fun (args: Expr list) -> 
    let qargs = args |> List.map(fun arg -> Expr.Coerce(arg, typeof<obj>)) |> (fun nargs -> Expr.NewArray(typeof<obj>, nargs)) 
    if selector = "*" then 
        <@@ (Fable.Core.JsInterop.importAll path : Fable.Core.JsInterop.JsFunc).Invoke(%%qargs) @@>
    elif selector = "default" then 
        <@@ (Fable.Core.JsInterop.importDefault path : Fable.Core.JsInterop.JsFunc).Invoke(%%qargs)  @@>
    else 
        <@@ (Fable.Core.JsInterop.import selector path : Fable.Core.JsInterop.JsFunc).Invoke(%%qargs) @@>

let makeCtor(fableLibVersion, args) =
    ProvidedConstructor(args, 
        invokeFableObjFn(fableLibVersion, args |> List.map(fun n -> n.Name))
    )

let makeImportMethod(name: string, params': ProvidedParameter list, returnType: System.Type, isStatic: bool, path: string, selector: string) =
    ProvidedMethod(
        name, 
        params',
        returnType,
        false,
        invokeFableFn(path, selector),
        isStatic = isStatic
    )
