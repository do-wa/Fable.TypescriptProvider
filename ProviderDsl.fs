module rec ProviderDsl

open System.Reflection
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes


type ErasedType =
    | Any
    | Bool
    | Int
    | Float
    | String
    | Array of ErasedType
    | Option of ErasedType
    | Custom of System.Type


let makeType = function
    | Any -> typeof<obj>
    | Bool -> typeof<bool>
    | Int -> typeof<int>
    | Float -> typeof<float>
    | String -> typeof<string>
    | Array t -> (makeType t).MakeArrayType()
    | Option t -> typedefof<Option<obj>>.MakeGenericType(makeType t)
    | Custom t -> t

let makeProperty(name: string, type': ErasedType) = 
    ProvidedProperty(name, makeType type', (fun args -> args.[0]))

let makeInterfaceTypeWithMembers (name: string, members: #MemberInfo list) =
    let t = ProvidedTypeDefinition(name, baseType = Some typeof<obj>, hideObjectMethods = true, IsErased = true, isInterface = true)
    
    t.AddMembers(members)
    t

let makeInterfaceType(name: string) =
    ProvidedTypeDefinition(name, baseType = Some typeof<obj>, hideObjectMethods = true, IsErased = true, isInterface = true)

let makeMethod(name: string, params': (string * ErasedType) list, returnType: ErasedType) = 
    ProvidedMethod(
        name, 
        params' |> List.map(fun (name, type') -> ProvidedParameter(name, makeType type')),
        makeType returnType
    )

//let makeCustomType(name: string, members: Member list, isInterface: bool): System.Type =
//    let t = ProvidedTypeDefinition(name, baseType = Some typeof<obj>, hideObjectMethods = true, isErased = true, isInterface = isInterface)
//    addMembers t members
//    upcast t

let makeRootType(assembly: Assembly, nameSpace: string, typeName: string, members: #MemberInfo list) =
    let root = ProvidedTypeDefinition(assembly, nameSpace, typeName, baseType = Some typeof<obj>, hideObjectMethods = true, IsErased = true)
    root.AddMembers members
    root
