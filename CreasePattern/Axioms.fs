namespace CreasePattern

open Geometry

type Component =
    | VertexComponent of Point2D
    | EdgeComponent of Edge


type Axiom =
    | First
    | Second
    | Third

module Axioms =

    let first v1 v2 : Line2D option = Line2D.fromTo v1 v2

    let second v1 v2 : Line2D option =
        Line2D.fromTo v1 v2
        |> Option.map (Line2D.perpThroughPoint (Point2D.midpoint v1 v2))

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
                (Point2D.midpoint p (Line2D.pointOnLine p l2))

            match Line2D.intersection l1 l2 with
            (* 2. Lines are parallel*)
            | None ->
                Line2D.fromTo (midpoint l1.startPoint) (midpoint l1.endPoint)
                |> Option.toList

            (* 3. Lines are intersecting *)
            | Some intersection ->
                let linePoint =
                    if l1.startPoint = intersection then
                        l1.endPoint
                    else
                        l1.startPoint

                // We need to make sure that we get the acute and the obtuse angle bisectors
                Line2D.fromTo linePoint intersection
                |> Option.map
                    (fun line ->
                        [ line
                          Line2D.perpThroughPoint intersection line ])
                |> Option.defaultValue []






    let perform axiom comp1 comp2 =
        match axiom with
        | First ->
            match comp1, comp2 with
            | VertexComponent v1, VertexComponent v2 -> first v1 v2 |> Option.toList
            | _ -> []

        | Second ->
            match comp1, comp2 with
            | VertexComponent v1, VertexComponent v2 -> second v1 v2 |> Option.toList
            | _ -> []

        | Third ->
            match comp1, comp2 with
            | EdgeComponent e1, EdgeComponent e2 -> third e1.line e2.line
            | _ -> []
