module Gallery

[<Literal>]
let fableVersion = "3.0.1"

open Feliz
open Fable.TypescriptProvider
open Fable.Core 

open Fable.Core.JsInterop

type ShowCaseProps = {
    moduleName: string
    showCase: Fable.React.ReactElement
}


type LeftPad = Import<"default", "left-pad">
[<ReactComponent>]
let leftPadShowCase() = 
    Html.div [
        prop.children [
            Html.text (LeftPad.leftPad(U2.Case1 "bar", 4.0, Some(U2.Case1 "foo")))
        ]
    ]


type AwesomeButtonModule = Import<"AwesomeButton, AwesomeButtonProgress", "react-awesome-button">

let AwesomeButton props : Fable.React.ReactElement = unbox AwesomeButtonModule.AwesomeButton props
type AwesomeButtonProps = AwesomeButtonModule.AwesomeButtonProps

let awesomeButtonStyles : obj = importDefault "react-awesome-button/src/styles/styles.scss";

[<ReactComponent>]
let awesomeButtonShowcase() = 
    let onPress = (fun (x:obj) -> x) 
    Html.div [
        prop.children [
            AwesomeButton [
                AwesomeButtonProps.onPress(fun () -> Browser.Dom.window.alert(sprintf "On %s, you liked some TypeProvider!" (System.DateTime.Now.ToString())))
                AwesomeButtonProps.cssModule awesomeButtonStyles
                AwesomeButtonProps.type' "primary"
                unbox (prop.children [
                    Html.text "Who likes Typeprovider? "
                ])
            ]
        ]
    ]


[<ReactComponent>]
let ShowCase(props: ShowCaseProps) = Html.div [
    prop.className "flex p-4 flex-col"
    prop.children [ 
        Html.div [
            prop.children [
                Html.h3 [ 
                    prop.className "text-xl p-2 flex-1 bg-gray-100 font-semibold"
                    prop.text props.moduleName 
                ]
            ]
        ] 
        Html.div [ 
            prop.className "p-2 flex-1"
            prop.children [ props.showCase  ]
        ]
    ] 

]

[<ReactComponent>]
let Show() = Html.div [ 
        prop.className "flex p-4 flex-col" 
        prop.children [ 
                Html.h1 [
                        prop.className "font-bold text-2xl"
                        prop.text "Fable.Typescript Provider Compile Tests and Showcase Gallery" 
                    ] 
                ShowCase({ moduleName = "left-pad"; showCase = leftPadShowCase() })
                ShowCase({ moduleName = "react-awesome-button"; showCase = awesomeButtonShowcase() })
            ]
    ]