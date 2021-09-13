module WebApplication.Pages.NotFound

open Fulma
open Fable.React

let view () =
    Container.container [ Container.IsFluid
                          Container.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ] [
        Heading.h1 [] [ str "Page Not Found" ]
        Heading.h2 [ Heading.IsSubtitle ] [
            str "404: Could not locate the page you are looking for"
        ]
    ]
