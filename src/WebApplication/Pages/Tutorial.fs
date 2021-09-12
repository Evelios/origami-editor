module WebApplication.Pages.Tutorial

open Fulma

type Model = Model

type Msg = DoNothing

let init () = Model

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | DoNothing -> model

let view (model: Model) dispatch =
    Hero.hero [ Hero.Color IsSuccess
                Hero.IsFullHeight ] []
