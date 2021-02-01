module ts2fableUtils

open ts2fable
open ts2fable.Syntax


let languagePrimitiveTypes = ["string"; "bool"; "int"; "float"; "unit"; "char"; "obj"] |> Set.ofList // complete this list
let jsWorldTypes = ["React"; "React.ReactNode"; "Component"] |> Set.ofList

let rec flattenTypes (allTypes: FsType list) =
    let types = seq {
        for t in allTypes do 
            match t with 
            | FsType.Property p -> yield! flattenTypes [p.Type]
            | FsType.Interface i -> 
                yield FsType.Interface(i)
                yield! flattenTypes i.Members
            | FsType.Module m -> 
                yield FsType.Module(m)
                yield! flattenTypes m.Types
            | t -> yield t
    }
    types |> Seq.distinct |> Seq.toList

let findExportInterface = List.tryFind(function | FsType.Interface i -> i.Name = "IExports" | _ -> false)


let establishExportStructure (allTypes: FsType list) (export: FsInterface) =
    let methods = export.Members |> List.filter(function | FsType.Function _ -> true | _ -> false)
    let propertyUnderlyingTypes = 
        export.Members 
        |> List.choose(function | FsType.Property p -> Some p | _ -> None)
        |> List.map(fun p -> 
            match p.Type with 
            | FsType.Mapped map when languagePrimitiveTypes.Contains(map.name) -> FsType.Mapped(map)
            | FsType.Mapped map -> allTypes |> List.)
        