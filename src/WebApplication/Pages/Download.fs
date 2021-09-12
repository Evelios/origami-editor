module WebApplication.Pages.Download

open Fulma

type Model = Model

type Msg = NoMsg

let init() = Model

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | NoMsg -> model

let view (model: Model) dispatch =
    Hero.hero [ Hero.Color IsSuccess
                Hero.IsFullHeight ] []
