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

type ErasedType =
    | Any
    | Bool
    | Int
    | Float
    | String
    | Array of ErasedType
    | Option of ErasedType
    | Custom of System.Type


let mapErasedType = function
    | Any -> typeof<obj>
    | Bool -> typeof<bool>
    | Int -> typeof<int>
    | Float -> typeof<float>
    | String -> typeof<string>
    | Array t -> (mapErasedType t).MakeArrayType()
    | Option t -> typedefof<Option<obj>>.MakeGenericType(mapErasedType t)
    | Custom t -> t

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

let makeImportMethod(name: string, params': ProvidedParameter list, returnType: System.Type, isStatic: bool, path: string) =
    ProvidedMethod(
        name, 
        params',
        returnType,
        false,
        (fun args ->
                <@@ 
                    let x  : Fable.Core.JsInterop.JsFunc = Fable.Core.JsInterop.import name path 
                    x.Invoke("test")
                @@> 
            ),
        isStatic = isStatic
    )

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

let inline invokeFableFn (path: string) = fun (args: Expr list) -> <@@ (Fable.Core.JsInterop.importAll path : Fable.Core.JsInterop.JsFunc).Invoke(%(exprAsFnArgs args) |> List.toArray) @@>

let makeImportAllMethod(name: string, params': ProvidedParameter list, returnType: System.Type, isStatic: bool, path: string) =
    ProvidedMethod(
        name, 
        params',
        returnType,
        false,
        invokeFableFn(path),
        isStatic = isStatic
    )


//let makeImportAllMethod(name: string, params': ProvidedParameter list, returnType: System.Type, isStatic: bool, path: string) =
//    ProvidedMethod(
//        name, 
//        params',
//        returnType,
//        false,
//        (fun args -> 
//                    let fnArgs = exprAsFnArgs args
//                    <@@     
                        
//                        let data = Fable.Core.JsInterop.toPlainJsObj  (%fnArgs |> List.toArray)
                            
//                        //let args = (%arg)()
//                        //let inv : Fable.Core.JsInterop.JsFunc = (Fable.Core.JsInterop.importAll path : Fable.Core.JsInterop.JsFunc)
//                        //inv.Invoke(test)
//                        (Fable.Core.JsInterop.importAll path : Fable.Core.JsInterop.JsFunc).Invoke(data)
//                     @@>
//                    //let x = (%%args:obj[]).[i]
//                    //let f = 
//                    //     (Fable.Core.JsInterop.importAll path : Fable.Core.JsInterop.JsFunc).Invoke((%%args.[0]:obj)) 

                     
                 
//                    //<@@ 
//                    //    let x = <@@ [for arg in args do (%%arg)] @@>
//                    //    printfn "%O" x 
//                    // @@>
//                    //let fn = <@ (Fable.Core.JsInterop.importAll path : Fable.Core.JsInterop.JsFunc) @>
//                    //let y = <@ (%fn : Fable.Core.JsInterop.JsFunc).Invoke([for arg in args do  if arg.Type.IsAssignableFrom(typeof<string>) then box (%%arg:string) else box (%%arg:float) ]) @>
//                    //<@@ y @@>
//                    //   [ 1 .. 4 ] |> List.fold (fun st n -> <@ n + %st @>) <@ 0 @>
//                    //let args = args 
//                    //           |> List.map(fun arg -> 
//                    //                if arg.Type.IsAssignableFrom(typeof<string>) then box (%%arg: string)
//                    //                elif arg.Type.IsAssignableFrom(typeof<float>) then box (%%arg: float)
//                    //                else box (%%arg:obj))
//                    //let y = args |> List.fold (fun f n -> <@ n::%f @>) <@ [] @>
//                    //<@@
//                    //let x = [for arg in args do  if arg.Type.IsAssignableFrom(typeof<string>) then box (%%arg:string) else box (%%arg:float) ] 
//                    //(Fable.Core.JsInterop.importAll path : Fable.Core.JsInterop.JsFunc).Invoke() @@>
//                    //(Fable.Core.JsInterop.importAll path : Fable.Core.JsInterop.JsFunc).Invoke([for arg in args do if arg.Type.IsAssignableFrom(typeof<string>) then box (%%arg:string) else box (%%arg:float)])
//                    //(Fable.Core.JsInterop.importAll path : Fable.Core.JsInterop.JsFunc).Invoke([for arg in args do if arg.Type.IsAssignableFrom(typeof<string>) then box (%%arg:string) else box (%%arg:float)])
               

//                //x.Invoke("Test",10.0,"Test")
//           // @@>
//          ),
//        isStatic = isStatic
//    )

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
