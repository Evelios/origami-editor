namespace CreasePattern

open Geometry

type GraphElement =
    | VertexElement of Point2D
    | EdgeElement of Line2D

[<RequireQualifiedAccess>]
type Axiom =
    | One of Point2D * Point2D
    | Two of Point2D * Point2D
    | Three of Line2D * Line2D


module Axiom =

    open Utilities.Extensions

    (* Builders *)

    /// Get all the axioms that can be performed between the two graph elements
    let betweenElements e1 e2 : Axiom list =
        match e1, e2 with
        | VertexElement p1, VertexElement p2 -> [ Axiom.One(p1, p2); Axiom.Two(p1, p2) ]
        | EdgeElement l1, EdgeElement l2 -> [ Axiom.Three(l1, l2) ]
        | _ -> []

    /// Get the list of axioms that can be performed on a list of graph elements
    let fromElements elements : Axiom list =
        List.pairs elements
        |> List.map (Tuple2.uncurry betweenElements)
        |> List.concat


    (* Actions *)

    let private first v1 v2 : Line2D = Line2D.fromTo v1 v2

    let private second v1 v2 : Line2D =
        Line2D.fromTo v1 v2
        |> Line2D.perpThroughPoint (Point2D.midpoint v1 v2)

    (* Try to fold two lines through the angle bisectors. This has three possible cases.
       1. The lines are the same -> No new folds
       2. The lines are parallel -> One fold between the two folds
       3. The lines are intersecting -> Two possible folds, the acute angle bisector and the obtuse angle bisector
    *)
    let private third l1 l2 : Line2D list =
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

    /// Get the result of performing an axiom action
    let perform action : Line2D list =
        match action with
        | Axiom.One (p1, p2) -> [ first p1 p2 ]
        | Axiom.Two (p1, p2) -> [ second p1 p2 ]
        | Axiom.Three (l1, l2) -> third l1 l2
