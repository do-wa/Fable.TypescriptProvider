module App

open Feliz
open Fable
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.JsInterop


type LeftPad = Fable.TypescriptProvider.Import<"left-pad">

let x = createObj (LeftPad.TestType.Props("Test"))

//type AwesomeButtom = Fable.TypescriptProvider.Import<"react-awesome-button">

//let y: AwesomeButtom.AwesomeButtonProps = !!{| action = "" |} // geht nicht.. keine Type fehler

//let btn props = AwesomeButtom.AwesomeButton(props)
//let pad = LeftPad.leftPad

//btn z |> ignore

[<ReactComponent>]
let Counter() =
    let (count, setCount) = React.useState(0)
    Html.div [
        Html.button [
            prop.style [ style.marginRight 5 ]
            prop.onClick (fun _ -> setCount(count + 1))
            prop.text "Increment"
        ]

        Html.button [
            prop.style [ style.marginLeft 5 ]
            prop.onClick (fun _ -> setCount(count - 1))
            prop.text "Decrement"
        ]

        Html.h1 count
    ]

open Browser.Dom

ReactDOM.render(Counter(), document.getElementById "root")