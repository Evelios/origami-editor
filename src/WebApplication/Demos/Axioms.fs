module WebApplication.Demos.Axioms

open Feliz
open Feliz.Bulma
open Browser

open Origami
open Geometry
open Utilities.Extensions
open WebApplication
open WebApplication.Demos

type Model =
    { Axiom: Axiom
      AxiomAction: AxiomAction
      Hover: GraphElement option }

type Msg =
    | ToggleAxiom of Axiom
    | MouseMove of Point2D

let initAxiom axiom =
    match axiom with
    | Axiom.One -> AxiomAction.One((Point2D.xy 50. 125.), (Point2D.xy 225. 75.))
    | Axiom.Two -> AxiomAction.Two((Point2D.xy 125. 25.), (Point2D.xy 75. 250.))
    | Axiom.Three ->
        AxiomAction.Three(
            (Line2D.through (Point2D.xy 50. 25.) (Point2D.xy 225. 225.)),
            (Line2D.through (Point2D.xy 25. 150.) (Point2D.xy 300. 200.))
        )

let init () =
    let axiom = Axiom.One

    { Axiom = axiom
      AxiomAction = initAxiom axiom
      Hover = None }

let update (msg: Msg) (model: Model) : Model =
    match msg with
    | ToggleAxiom axiom ->
        { model with
              Axiom = axiom
              AxiomAction = initAxiom axiom }
    | MouseMove position ->
        console.log position

        model

let axiomDemo model dispatch =
    let boundingBox =
        BoundingBox2D.withDimensions Point2D.origin 400. 300.

    let axiomResults =
        Axiom.perform model.AxiomAction
        |> Seq.map (fun line -> Draw.line line boundingBox [ svg.stroke Theme.colors.Success ])
        |> Seq.filterNone

    let axiomActions =
        match model.AxiomAction with
        | AxiomAction.One (p1, p2) ->
            [ Draw.point p1 [ svg.fill Theme.colors.Info ]
              Draw.point p2 [ svg.fill Theme.colors.Info ] ]
        | AxiomAction.Two (p1, p2) ->
            [ Draw.point p1 [ svg.fill Theme.colors.Info ]
              Draw.point p2 [ svg.fill Theme.colors.Info ] ]
        | AxiomAction.Three (l1, l2) ->
            [ yield!
                Draw.line l1 boundingBox [ svg.stroke Theme.colors.Info ]
                |> Option.toList
              yield!
                  Draw.line l2 boundingBox [ svg.stroke Theme.colors.Info ]
                  |> Option.toList
              Draw.point l1.Start [ svg.fill Theme.colors.Info ]
              Draw.point l1.Finish [ svg.fill Theme.colors.Info ]
              Draw.point l2.Start [ svg.fill Theme.colors.Info ]
              Draw.point l2.Finish [ svg.fill Theme.colors.Info ] ]

    Svg.svg [ svg.viewBox (
                  int boundingBox.MinX,
                  int boundingBox.MinY,
                  int <| BoundingBox2D.width boundingBox,
                  int <| BoundingBox2D.height boundingBox
              )
              svg.width (BoundingBox2D.width boundingBox)
              svg.height (BoundingBox2D.height boundingBox)
              svg.children [ yield! axiomResults
                             yield! axiomActions ]
              svg.onMouseMove (fun e -> MouseMove(Point2D.xy e.x e.y) |> dispatch) ]

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
                  if axiom = model.Axiom then
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
