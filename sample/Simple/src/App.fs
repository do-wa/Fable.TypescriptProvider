module App

open Browser.Dom
open Fable.Core.JsInterop
open Fable.Core

type LeftPadModule = Fable.TypescriptProvider.Import<"left-pad">
let leftPad = LeftPadModule.LeftPad.leftPad

let y () = 
    let x = leftPad("",0.0,"")
    printfn "%s" x
    x

//let leftPad : LeftPad = LeftPadModule.LeftPad.leftPad("",0.0,"")
//let x () = LeftPadModule.LeftPad.leftPad("",0.0,"")

//let x : LeftPadModule.LeftPad.leftPad = y
//let test = x.leftPad("",0.0,"")

//let test () = y("",0.0,"")
//let x : LeftPadModule.LeftPad.TestImpl = jsNative
//let y = x.leftPad("",0.0,"")
//[<ImportDefault("left-pad")>]
//let leftPad = LeftPadModule.LeftPad.leftPad

//let y () = leftPad("",0.0,"")

//let x : LeftPadModule.LeftPad = jsNative


//let xy = x.leftPad("",0.0,"")
//let x = 0

// Mutable variable to count the number of times we clicked the button
let mutable count = 0

// Get a reference to our button and cast the Element to an HTMLButtonElement
let myButton = document.querySelector(".my-button") :?> Browser.Types.HTMLButtonElement

myButton.onclick <- fun _ ->
    count <- count + 1  
    myButton.innerText <- sprintf "Count: %i Padded Text: %s time(s)" count (y())
