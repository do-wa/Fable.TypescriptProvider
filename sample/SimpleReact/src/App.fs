module App

open Feliz
open Fable
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.JsInterop

type LeftPad = Fable.TypescriptProvider.Import<"left-pad">
type AwesomeButtom = Fable.TypescriptProvider.Import<"react-lite-button">





//let y: AwesomeButtom.AwesomeButtonProps = !!{| action = "" |} // geht nicht.. keine Type fehler

//let btn props = AwesomeButtom.AwesomeButton(props)
//let pad = LeftPad.leftPad

//btn z |> ignore

[<ReactComponent>]
let Counter() =
    let (count, setCount) = React.useState(0)

    let btnProps = createObj(AwesomeButtom.AwesomeButtonProps.Props(true, false, "Test", "Test", false, "Test", false, false, false, "Test", "", "", "Test","Test","","",false))

    Html.div [
        !!AwesomeButtom.AwesomeButton(!!btnProps)
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