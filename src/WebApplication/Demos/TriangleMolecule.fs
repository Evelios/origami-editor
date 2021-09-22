module WebApplication.Demos.TriangleMolecule

open Feliz
open Feliz.Bulma

open Geometry

type Model = { R1: float; R2: float; R3: float }

type Msg =
    | ChangeRadii1 of float
    | ChangeRadii2 of float
    | ChangeRadii3 of float

let init () = { R1 = 150.; R2 = 70.; R3 = 100. }


let update (msg: Msg) (model: Model) : Model =
    match msg with
    | ChangeRadii1 r1 -> { model with R1 = r1 }
    | ChangeRadii2 r2 -> { model with R2 = r2 }
    | ChangeRadii3 r3 -> { model with R3 = r3 }

let private boundingBox =
    BoundingBox2D.from Point2D.origin (Point2D.xy 400. 300.)

/// Create a triangle from three segment lengths
let triangleFromRadii (r1, r2, r3) =
    let p1 = Point2D.origin

    let p2 = p1 + (Vector2D.xy r1 0.)

    let intersections =
        Intersection2D.circles (Circle2D.from p1 r3) (Circle2D.from p2 r2)

    match Seq.tryHead intersections with
    | None -> None
    | Some p3 ->

        let triangle = Triangle2D.from p1 p2 p3

        let triangleCenter =
            triangle
            |> Triangle2D.boundingBox
            |> BoundingBox2D.center

        triangle
        |> Triangle2D.translate (BoundingBox2D.center boundingBox - triangleCenter)
        |> Some



let triangleMolecule model dispatch =
    let children =
        let radii =
            (model.R1 + model.R2, model.R2 + model.R3, model.R3 + model.R1)

        match triangleFromRadii radii with
        | None -> List.empty
        | Some triangle -> [ Draw.triangle triangle [] ]

    Svg.svg [ svg.viewBox (
                  int boundingBox.MinX,
                  int boundingBox.MinY,
                  int <| BoundingBox2D.width boundingBox,
                  int <| BoundingBox2D.height boundingBox
              )
              svg.width (BoundingBox2D.width boundingBox)
              svg.height (BoundingBox2D.height boundingBox)
              svg.children children ]

let view (model: Model) dispatch =
    let radii =
        [ "r1", model.R1, ChangeRadii1, color.isSuccess
          "r2", model.R2, ChangeRadii2, color.isPrimary
          "r3", model.R3, ChangeRadii3, color.isInfo ]

    let sliders =
        let slider (name: string, value: float, msg, color: IReactProperty) =
            Bulma.field.div [ Bulma.label name
                              Bulma.control.div [ Slider.slider [ color
                                                                  prop.value value
                                                                  prop.max 150
                                                                  prop.min 10
                                                                  prop.onChange
                                                                      (fun (i: int) -> msg (float i) |> dispatch) ] ] ]

        Html.form [ field.isGrouped
                    text.hasTextCentered
                    prop.children (Bulma.fieldBody (List.map slider radii)) ]


    Html.div [ triangleMolecule model dispatch
               sliders ]
