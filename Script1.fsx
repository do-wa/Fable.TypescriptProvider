#r "./bin/Debug/netstandard2.0/Fable.TypescriptProvider.dll"

type X = Fable.TypescriptProvider.Generator<"Bla">

let testCall (test: X.test2.test.TooltipProps) = 
    test


let y = testCall { afterHide = ""}