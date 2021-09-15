module WebApplication.Shell

open Elmish
open Router
open Feliz
open Feliz.Bulma

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
    | TutorialMsg of Tutorial.Msg
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

let update (msg: Msg) (model: Model) =
    match msg, model.ActivePage with
    | HomeMsg homeMsg, Page.Home homeModel ->
        { model with
              ActivePage = Home.update homeMsg homeModel |> Page.Home },
        Cmd.none
    | TutorialMsg tutorialMsg, Page.Tutorial tutorialModel ->
        { model with
              ActivePage =
                  Tutorial.update tutorialMsg tutorialModel
                  |> Page.Tutorial },
        Cmd.none
    | _ -> model, Cmd.none

let view (model: Model) (dispatch: Msg -> Unit) =
    let headerTabLinks =
        [ ("Home", Route.Home)
          ("Tutorial", Route.Tutorial)
          ("Examples", Route.Examples)
          ("Download", Route.Download) ]

    let heroSize =
        match model.ActivePage with
        | Page.NotFound
        | Page.Home _ -> hero.isFullHeight
        | _ -> hero.isMedium

    let heroColor =
        match model.ActivePage with
        | Page.NotFound -> color.isDanger
        | Page.Home _ -> color.isSuccess
        | _ -> color.isInfo

    let page =
        match model.ActivePage with
        | Page.NotFound -> NotFound.view ()
        | Page.Loading -> Home.view (Home.init ()) dispatch
        | Page.Home homeModel -> Home.view homeModel dispatch
        | Page.Tutorial tutorialModel -> Tutorial.view tutorialModel (TutorialMsg >> dispatch)
        | Page.Examples examplesModel -> Examples.view examplesModel dispatch
        | Page.Download downloadModel -> Download.view downloadModel dispatch

    let navbar =
        let icon =
            Html.img [ prop.src "icons/fold.svg"
                       prop.height 28
                       prop.width 28 ]

        let brand =
            [ Bulma.navbarItem.a [ prop.href (toHash Route.Home)
                                   prop.children [ icon ] ] ]

        let links =
            let link (name: string, route) =
                Bulma.navbarItem.a [ if Some route = model.CurrentRoute then
                                         navbarItem.isActive
                                     prop.text name
                                     prop.href (toHash route) ]

            List.map link headerTabLinks


        Bulma.navbar [ heroColor
                       navbar.isFixedTop
                       prop.children [ Bulma.navbarBrand.div brand
                                       Bulma.navbarMenu [ Bulma.navbarStart.div links ] ] ]

    let hero =
        let title = Bulma.title page.title
        let subtitle = Bulma.subtitle page.subtitle

        let heroBody =
            Bulma.container [ container.isFluid
                              text.hasTextCentered
                              prop.children [ title; subtitle ] ]

        Bulma.hero [ heroColor
                     heroSize
                     prop.children [ Bulma.heroHead navbar
                                     Bulma.heroBody heroBody ] ]

    Html.div [ hero; page.body ]
