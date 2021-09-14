module WebApplication.Demos.Axioms

open Feliz
open Feliz.Bulma
open Origami

type Model =
    { axiom : Axiom
    }

type Msg = NoMsg

let init () =
    { axiom = Axiom.One
    }

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | NoMsg -> model

let view (model: Model) dispatch =
    Bulma.container
        [ Html.h1 "Axioms"
        ]
    