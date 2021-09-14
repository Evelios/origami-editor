module WebApplication.Pages.Tutorial

open WebApplication
open WebApplication.Demos

type Model = Axioms.Model

type Msg = DoNothing

let init () = Axioms.init ()

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | DoNothing -> model

let view model dispatch =
    { title = "Tutorial Page"
      subtitle = ""
      body = Axioms.view model dispatch }
