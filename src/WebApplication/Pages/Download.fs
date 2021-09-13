module WebApplication.Pages.Download

open Feliz

open WebApplication

type Model = Model

type Msg = NoMsg

let init () = Model

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | NoMsg -> model

let view model dispatch =
    { title = "Download the Origami Editor"
      subtitle = ""
      body = Html.div [] }
