module App

open Browser.Dom
open Fable.Core.JsInterop
open Fable.Core


type LeftPad = Fable.TypescriptTypeProvider.Import<"default","left-pad", "3.1.1">
let padded = LeftPad.leftPad(U2.Case1 "Test", 0.0, None)


//let leftPad = LeftPadModule.leftPad

//leftPad() |> ignore

//let padded = leftPad("Text", 50.0, "A")
//printfn "%s" padded

let mutable count = 0

// Get a reference to our button and cast the Element to an HTMLButtonElement
let myButton = document.querySelector(".my-button") :?> Browser.Types.HTMLButtonElement

myButton.onclick <- fun _ ->
    count <- count + 1  
   // myButton.innerText <- sprintf "Count: %i Padded Text: %s time(s)" count (leftPad("Test",float(count),"Juhu!"))
