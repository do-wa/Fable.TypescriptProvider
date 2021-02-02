module App

open Browser.Dom
open Fable.Core.JsInterop
open Fable.Core

type LeftPad = Fable.TypescriptProvider.Import<"default", "left-pad", "3.1.1">

let paddedString = LeftPad.leftPad(U2.Case1 "TESTS", 5.0, Some(U2.Case1 " ME NEED MORE "))



let myButton = document.querySelector(".my-button") :?> Browser.Types.HTMLButtonElement

myButton.onclick <- fun _ ->
    myButton.innerText <- sprintf "Padded String: %s" paddedString
