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


type LeftPad = Import<"default", "left-pad", fableVersion>
[<ReactComponent>]
let leftPadShowCase() = 
    Html.div [
        prop.children [
            Html.text (LeftPad.leftPad(U2.Case1 "bar", 4.0, Some(U2.Case1 "foo")))
        ]
    ]

type AwesomeButtonModule = Import<"AwesomeButton", "react-awesome-button", fableVersion>
let AwesomeButton props : Fable.React.ReactElement = unbox (AwesomeButtonModule.AwesomeButton(props))
let styling = importDefault "react-awesome-button/src/styles/styles.scss"
let awesomeButtonShowcase() =
    let props = AwesomeButtonModule.AwesomeButton.AwesomeButtonProps(null,false, false,
                 null,null, styling ,false, null, "", false,
                 null, null, true, true, "",  "12", null, null
                 ,  "Awesome!", "To?",null,true)
    Html.div [
        prop.children [
            AwesomeButton(props)
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