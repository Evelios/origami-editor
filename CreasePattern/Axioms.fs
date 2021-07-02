namespace CreasePattern

open Geometry

type Component =
    | VertexComponent of Point2D
    | EdgeComponent of Edge


type Axiom = | First

module Axioms =

    let first v1 v2 = Line2D.fromTo v1 v2

    let perform axiom comp1 comp2 =
        match axiom with
        | First ->
            match comp1, comp2 with
            | VertexComponent v1, VertexComponent v2 -> Some(first v1 v2)

            | _ -> None
