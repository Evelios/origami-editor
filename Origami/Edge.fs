[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.Edge

open Math.Geometry
open Math.Units
open Utilities.Extensions


// ---- Builders -----------------------------------------------------------

/// Create an edge from a line segment or crease location and it's edge assignment.
let atWithAssignment (crease: LineSegment2D<Meters, 'Coordinates>) (assignment: EdgeAssignment) : Edge<'Coordinates> =
    { Crease = crease
      Assignment = assignment }

/// Create an edge between two points and give it an edge assignment.
let betweenWithAssignment
    (start: Point2D<Meters, 'Coordinates>)
    (finish: Point2D<Meters, 'Coordinates>)
    (assignment: EdgeAssignment)
    : Edge<'Coordinates> =
    atWithAssignment (LineSegment2D.from start finish) assignment


// ---- Modifiers ----------------------------------------------------------

/// Scale the edge by a scalar with the reference point at the origin.
let scale (x: float) (edge: Edge<'Coordinates>) : Edge<'Coordinates> = { edge with Crease = edge.Crease * x }

/// Get the distance between a point and an edge.
let distanceToVertex (vertex: Point2D<Meters, 'Coordinates>) (edge: Edge<'Coordinates>) : Length =
    edge.Crease
    |> LineSegment2D.distanceToPoint vertex

/// <summary>
///   Round the floating point. The rounding works the same way as the
///   <see cref="T:Math.Units.Quantity.round"/> function.
/// </summary>
///
let round (edge: Edge<'Coordinates>) : Edge<'Coordinates> =
    { edge with Crease = LineSegment2D.round edge.Crease }


// ---- Accessors ----------------------------------------------------------

/// Get the vertices of the edge. This is the start and end point for the edge crease.
let vertices (edge: Edge<'Coordinates>) : Point2D<Meters, 'Coordinates> * Point2D<Meters, 'Coordinates> =
    edge.Crease.Start, edge.Crease.Finish

/// Get all the endpoints points of of some edges.
let seqVertices (edges: Edge<'Coordinates> seq) : Point2D<Meters, 'Coordinates> seq =
    Seq.map vertices edges
    |> Seq.map Tuple2.toList
    |> Seq.concat

/// Get the edge crease as a line continuing infinitely before and after the edge crease.
let line (e: Edge<'Coordinates>) : Line2D<Meters, 'Coordinates> = e.Line

/// Get the edge assignment.
let assignment (e: Edge<'Coordinates>) : EdgeAssignment = e.Assignment
