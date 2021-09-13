module WebApplication.Pages.Tutorial

open Feliz

open WebApplication

type Model = Model

type Msg = DoNothing

let init () = Model

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | DoNothing -> model

let view model dispatch =
    { title = "Tutorial Page"
      subtitle = ""
      body = Html.div [] }
