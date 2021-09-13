module WebApplication.Pages.Home

open WebApplication

open Feliz

type Model = Model

type Msg = Msg

let init () = Model

let update msg state =
    match msg with
    | Msg -> state

let view model dispatch : PageView =
    { title = "OrigamiEditor"
      subtitle = "The best way to fold on the computer"
      body = Html.div [] }
