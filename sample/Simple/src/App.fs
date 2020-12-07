module App

open Browser.Dom
open Fable.Core.JsInterop
open Fable.Core
type LeftPad = Fable.TypescriptProvider.Generator<"left-pad   ">

type ITest = 
    abstract leftPad: (string *float * string) -> string

[<ImportDefault("left-pad")>]
let mylib2: ITest = jsNative
let test3 () = mylib2.leftPad("Test", 0.0, "Test3")

[<ImportDefault("left-pad")>]
let mylib: LeftPad.``left-pad``.IExports= jsNative

let test () = mylib.leftPad("test",0.0,"")

let leftPadModule : LeftPad.``left-pad``.IExports = importDefault "left-pad" 
let test2 = leftPadModule.leftPad("test",0.0,"test")

// Mutable variable to count the number of times we clicked the button
let mutable count = 0

// Get a reference to our button and cast the Element to an HTMLButtonElement
let myButton = document.querySelector(".my-button") :?> Browser.Types.HTMLButtonElement

// Register our listener
myButton.onclick <- fun _ ->
    count <- count + 1
    //myButton.innerText <- sprintf "Padded Text: %s time(s)" (leftPadModule.leftPad("", float(count), ""))
