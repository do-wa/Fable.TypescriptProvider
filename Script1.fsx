#r "./bin/Debug/netstandard2.0/Fable.TypescriptProvider.dll"

type X = Fable.TypescriptProvider.Generator<"Bla">

let testCall (test: X.test.TooltipProps) = 
    


let y = testCall { afterHide = ""}