module WebApplication.Pages.Tutorial

open Feliz
open Feliz.Bulma

open WebApplication
open WebApplication.Demos

type Model =
    { Axioms: Axioms.Model
      TriangleMolecule: TriangleMolecule.Model }

type Msg =
    | AxiomsMsg of Axioms.Msg
    | TriangleMoleculeMsg of TriangleMolecule.Msg

let init () =
    { Axioms = Axioms.init ()
      TriangleMolecule = TriangleMolecule.init () }

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | AxiomsMsg axiomMsg ->
        { model with
              Axioms = Axioms.update axiomMsg model.Axioms }
    | TriangleMoleculeMsg triangleMoleculeMsg ->
        { model with
              TriangleMolecule = TriangleMolecule.update triangleMoleculeMsg model.TriangleMolecule }

let view (model: Model) (dispatch: Msg -> Unit) =
    let sectionData =
        [ ("Axioms", "The basic fold operations", Axioms.view model.Axioms (AxiomsMsg >> dispatch))
          ("Triangle Molecule",
           "The simplest of all the molecules",
           TriangleMolecule.view model.TriangleMolecule (TriangleMoleculeMsg >> dispatch)) ]

    let section (title: string, subtitle: string, demoView) =
        Bulma.section [ section.isMedium
                        prop.children [ Bulma.title title
                                        Bulma.subtitle subtitle
                                        Bulma.container [ text.hasTextCentered
                                                          container.isFluid
                                                          prop.children [ demoView ] ] ] ]

    { title = "Tutorial Page"
      subtitle = ""
      body = Bulma.container (List.map section sectionData) }
