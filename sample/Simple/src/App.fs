module App

open Browser.Dom
open Fable.Core.JsInterop
open Fable.Core


type LeftPad = Fable.TypescriptTypeProvider.Import<"default","left-pad">

let leftPad(test : LeftPad.leftPad) : LeftPad.IModule = importDefault "left-pad"

leftPad({ prop1 = ""}) |> ignore

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
