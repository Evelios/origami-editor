module WebApplication.Pages.Tutorial

open Feliz
open Feliz.Bulma

open WebApplication
open WebApplication.Demos

type Model = { Axioms: Axioms.Model }

type Msg = AxiomsMsg of Axioms.Msg

let init () = { Axioms = Axioms.init () }

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | AxiomsMsg axiomMsg ->
        { model with
              Axioms = Axioms.update axiomMsg model.Axioms }

let view (model: Model) (dispatch: Msg -> Unit) =
    { title = "Tutorial Page"
      subtitle = ""
      body =
          Bulma.section [ Bulma.title "Axioms"
                          Bulma.subtitle "The basic fold operations"

                          Bulma.container [ text.hasTextCentered
                                            container.isFluid
                                            prop.children [ Axioms.view model.Axioms (AxiomsMsg >> dispatch) ] ] ] }
