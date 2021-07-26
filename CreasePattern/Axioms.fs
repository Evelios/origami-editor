namespace CreasePattern

open Geometry

type Component =
    | VertexComponent of Point2D
    | EdgeComponent of Edge


[<RequireQualifiedAccess>]
type Axiom =
    | First
    | Second
    | Third

module Axioms =

    let first v1 v2 : Line2D = Line2D.fromTo v1 v2

    let second v1 v2 : Line2D =
        Line2D.fromTo v1 v2
        |> Line2D.perpThroughPoint (Point2D.midpoint v1 v2)

    (* Try to fold two lines through the angle bisectors. This has three possible cases.
       1. The lines are the same -> No new folds
       2. The lines are parallel -> One fold between the two folds
       3. The lines are intersecting -> Two possible folds, the acute angle bisector and the obtuse angle bisector
    *)
    let third l1 l2 : Line2D list =
        (* 1. Lines are equal *)
        if l1 = l2 then
            []

        else
            let midpoint p =
                (Point2D.midpoint p (Line2D.pointClosestTo p l2))

            match Line2D.intersection l1 l2 with
            (* 2. Lines are parallel*)
            | None -> [ Line2D.fromTo (midpoint l1.start) (midpoint l1.finish) ]

            (* 3. Lines are intersecting *)
            | Some intersection ->
                let linePoint =
                    if l1.start = intersection then
                        l1.finish
                    else
                        l1.start

                // We need to make sure that we get the acute and the obtuse angle bisectors
                let acuteLine = Line2D.fromTo linePoint intersection

                [ Line2D.perpThroughPoint intersection acuteLine
                  acuteLine ]

    let perform axiom comp1 comp2 : Line2D list =
        match axiom with
        | Axiom.First ->
            match comp1, comp2 with
            | VertexComponent v1, VertexComponent v2 -> [ first v1 v2 ]
            | _ -> []

        | Axiom.Second ->
            match comp1, comp2 with
            | VertexComponent v1, VertexComponent v2 -> [ second v1 v2 ]
            | _ -> []

        | Axiom.Third ->
            match comp1, comp2 with
            | EdgeComponent e1, EdgeComponent e2 -> third e1.line e2.line
            | _ -> []
