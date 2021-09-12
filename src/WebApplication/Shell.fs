module WebApplication.Shell

open Elmish
open Router

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

let init _ =
    { CurrentRoute = None
      ActivePage = Page.Home <| Home.init () },
    Cmd.none

let update msg state =
    match msg with
    | HomeMsg homeMsg -> Home.update homeMsg state, Cmd.none
    | NoMsg -> state, Cmd.none

let view state dispatch =
    match state.ActivePage with
    | Page.NotFound -> Home.view (Home.init ()) dispatch
    | Page.Loading -> Home.view (Home.init ()) dispatch
    | Page.Home model -> Home.view model dispatch
    | Page.Tutorial model -> Tutorial.view model dispatch
    | Page.Examples model -> Examples.view model dispatch
    | Page.Download model -> Download.view model dispatch
