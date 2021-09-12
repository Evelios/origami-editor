namespace WebApplication.Pages



module Home =
    open Fulma
    open Fable.React

    type Model = Model

    type Msg = Msg

    let init () = Model

    let update msg state =
        match msg with
        | Msg -> state

    let view model dispatch =
        Hero.hero [ Hero.Color IsSuccess
                    Hero.IsFullHeight ] [
            Hero.head [] [
                Tabs.tabs [ Tabs.IsBoxed; Tabs.IsCentered ] [
                    Tabs.tab [ Tabs.Tab.IsActive true ] [
                        a [] [ str "Home" ]
                    ]
                    Tabs.tab [] [ a [] [ str "Tutorial" ] ]
                    Tabs.tab [] [ a [] [ str "Examples" ] ]
                    Tabs.tab [] [ a [] [ str "Download" ] ]
                ]
            ]
            Hero.body [] [
                Container.container [ Container.IsFluid
                                      Container.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ] [
                    Heading.h1 [] [ str "Origami Editor" ]
                    Heading.h2 [ Heading.IsSubtitle ] [
                        str "The best way to fold on the computer"
                    ]
                ]
            ]
        ]
