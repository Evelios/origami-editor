[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.Axiom

open Utilities.Extensions
open Math.Geometry
open Math.Units


// ---- Builders ---------------------------------------------------------------

/// Get all the axioms that can be performed between the two graph elements
let betweenElements (e1: GraphElement<'Coordinates>) (e2: GraphElement<'Coordinates>) : AxiomAction<'Coordinates> list =
    match e1, e2 with
    | VertexElement p1, VertexElement p2 ->
        [ AxiomAction<'Coordinates>.One (p1, p2)
          AxiomAction<'Coordinates>.Two (p1, p2) ]
    | EdgeElement e1, EdgeElement e2 -> [ AxiomAction<'Coordinates>.Three (e1.Line, e2.Line) ]
    | _ -> []

/// Get the list of axioms that can be performed on a list of graph elements
let fromElements (elements: GraphElement<'Coordinates> seq) : AxiomAction<'Coordinates> seq =
    (List.ofSeq elements)
    |> List.pairs
    |> List.map (Tuple2.uncurry betweenElements)
    |> List.concat
    |> Seq.ofList

/// Get the list of axioms that can be performed on a list of graph elements from a set of axiom conditions
let ofAxiomsFromElements (axioms: Axiom seq) (elements: GraphElement<'Coordinates> seq): AxiomAction<'Coordinates> seq =
    fromElements elements
    |> Seq.filter (fun axiomAction ->
        match axiomAction with
        | AxiomAction.One _ -> Seq.contains Axiom.One axioms
        | AxiomAction.Two _ -> Seq.contains Axiom.Two axioms
        | AxiomAction.Three _ -> Seq.contains Axiom.Three axioms)


// ---- Actions ----------------------------------------------------------------

let first (v1: Point2D<Meters, 'Coordinates>) (v2: Point2D<Meters, 'Coordinates>) : Line2D<Meters, 'Coordinates> =
    Line2D.through v1 v2

let second (v1: Point2D<Meters, 'Coordinates>) (v2: Point2D<Meters, 'Coordinates>) : Line2D<Meters, 'Coordinates> =
    Line2D.through v1 v2
    |> Line2D.perpendicularThroughPoint (Point2D.midpoint v1 v2)

/// Try to fold two lines through the angle bisectors. This has three possible cases.
/// 1. The lines are the same -> No new folds
/// 2. The lines are parallel -> One fold between the two folds
/// 3. The lines are intersecting -> Two possible folds, the acute angle bisector and the obtuse angle bisector
///
let third (e1: Line2D<Meters, 'Coordinates>) (e2: Line2D<Meters, 'Coordinates>) : Line2D<Meters, 'Coordinates> list =
    (* 1. Lines are equal *)
    if e1 = e2 then
        []

    else
        let midpoint p =
            (Point2D.midpoint p (Line2D.pointClosestTo p e2))

        match Line2D.intersect e1 e2 with
        (* 2. Lines are parallel*)
        | None -> [ Line2D.through (midpoint e1.Start) (midpoint e1.Finish) ]

        (* 3. Lines are intersecting *)
        | Some intersection ->
            let linePoint =
                if e1.Start = intersection then
                    e1.Finish
                else
                    e1.Start

            // We need to make sure that we get the acute and the obtuse angle bisectors
            let acuteLine =
                Line2D.through linePoint intersection

            [ Line2D.perpendicularThroughPoint intersection acuteLine
              acuteLine ]

/// Get the result of performing an axiom action
let perform (action: AxiomAction<'Coordinates>) : Set<Line2D<Meters, 'Coordinates>> =
    match action with
    | AxiomAction.One (p1, p2) -> Set.ofList [ first p1 p2 ]
    | AxiomAction.Two (p1, p2) -> Set.ofList [ second p1 p2 ]
    | AxiomAction.Three (e1, e2) -> Set.ofList (third e1 e2)
