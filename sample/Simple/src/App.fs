module App

open Browser.Dom
open Fable.Core.JsInterop
open Fable.Core
type LeftPadModule = Fable.TypescriptProvider.Import<"default", "left-pad">
let leftPad = LeftPadModule.LeftPad.leftPad

let test = LeftPadModule.LeftPad.Props("",1, LeftPadModule.LeftPad.Nested())

let padded = leftPad(U2.Case1 "Text", 50.0, U2.Case1 "A")
printfn "%s" padded
 