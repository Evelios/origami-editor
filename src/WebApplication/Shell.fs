module WebApplication.Shell

open Elmish
open Fulma
open Router
open Fable.React
open Feliz

open WebApplication.Pages

type Page =
    | NotFound
    | Loading
    | Home of Home.Model
    | Tutorial of Tutorial.Model
    | Examples of Examples.Model
    | Download of Download.Model

type Msg =
    | HomeMsg of Home.Msg
    | NoMsg

type Model =
    { CurrentRoute: Route option
      ActivePage: Page }



let setRoute result model =
    let model = { model with CurrentRoute = result }

    match result with
    | None -> { model with ActivePage = NotFound }, Cmd.none

    | Some route ->
        match route with
        | Route.Home ->
            { model with
                  ActivePage = Page.Home <| Home.init () },
            Cmd.none
        | Route.Tutorial ->
            { model with
                  ActivePage = Page.Tutorial <| Tutorial.init () },
            Cmd.none
        | Route.Examples ->
            { model with
                  ActivePage = Page.Examples <| Examples.init () },
            Cmd.none
        | Route.Download ->
            { model with
                  ActivePage = Page.Download <| Download.init () },
            Cmd.none

(* Application Body *)

let init route =
    { CurrentRoute = None
      ActivePage = Page.Home <| Home.init () }
    |> setRoute route

let update msg state =
    match msg with
    | HomeMsg homeMsg -> Home.update homeMsg state, Cmd.none
    | NoMsg -> state, Cmd.none

let view model dispatch =
    let headerTabLinks =
        [ ("Home", Route.Home)
          ("Tutorial", Route.Tutorial)
          ("Examples", Route.Examples)
          ("Download", Route.Download) ]

    let heroSize =
        match model.ActivePage with
        | Page.NotFound
        | Page.Home _ -> Hero.IsFullHeight
        | _ -> Hero.IsHalfHeight

    let heroColor =
        match model.ActivePage with
        | Page.NotFound -> IsDanger
        | Page.Home _ -> IsSuccess
        | _ -> IsInfo

    let page =
        match model.ActivePage with
        | Page.NotFound -> NotFound.view ()
        | Page.Loading -> Home.view (Home.init ()) dispatch
        | Page.Home homeModel -> Home.view homeModel dispatch
        | Page.Tutorial tutorialModel -> Tutorial.view tutorialModel dispatch
        | Page.Examples examplesModel -> Examples.view examplesModel dispatch
        | Page.Download downloadModel -> Download.view downloadModel dispatch


    let linkTabs =
        let linkTab (name: string, route) =
            Tabs.tab [ Tabs.Tab.IsActive((Some route) = model.CurrentRoute) ] [
                Html.a [ prop.text name
                         prop.href (toHash route) ]
            ]

        List.map linkTab headerTabLinks

    Hero.hero [ Hero.Color heroColor; heroSize ] [
        Hero.head [] [
            Tabs.tabs [ Tabs.IsBoxed; Tabs.IsCentered ] linkTabs
        ]
        Hero.body [] [
            Container.container [ Container.IsFluid
                                  Container.Modifiers [ Modifier.TextAlignment(Screen.All, TextAlignment.Centered) ] ] [
                Heading.h1 [] [ str page.title ]
                Heading.h2 [ Heading.IsSubtitle ] [
                    str page.subtitle
                ]
            ]
        ]
    ]
