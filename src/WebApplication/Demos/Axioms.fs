module WebApplication.Demos.Axioms

open Feliz
open Feliz.Bulma

open Origami
open Geometry
open WebApplication.Demos

type Model = { axiom: Axiom }

type Msg = ToggleAxiom of Axiom

let init () = { axiom = Axiom.One }

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | ToggleAxiom axiom -> { model with axiom = axiom }

let axiomDemo model dispatch =
    let width = 200
    let height = 200

    Svg.svg [ svg.viewBox (0, 0, width, height)
              svg.width width
              svg.height height
              svg.children [ Draw.lineSegment (LineSegment2D.from (Point2D.xy 0. 0.) (Point2D.xy 200. 200.)) ] ]

let view (model: Model) dispatch =
    let axioms =
        [ Axiom.One, "icons/Axiom_1.svg"
          Axiom.Two, "icons/Axiom_2.svg"
          Axiom.Three, "icons/Axiom_3.svg" ]

    let buttons =
        let button (axiom, src) =
            let buttonAttributes =
                [ prop.onClick (fun _ -> ToggleAxiom axiom |> dispatch)
                  prop.children [ Html.img [ prop.src src
                                             prop.width 36
                                             prop.height 36 ] ]
                  if axiom = model.axiom then
                      button.isActive
                      Bulma.color.isSuccess
                  else
                      Bulma.color.isWarning ]

            Bulma.field.div [ Bulma.control.div (Bulma.button.button buttonAttributes) ]



        Html.form [ field.isGrouped
                    text.hasTextCentered
                    prop.children (Bulma.fieldBody (List.map button axioms)) ]


    Html.div [ axiomDemo model dispatch
               buttons ]
