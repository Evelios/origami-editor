namespace Origami

open System
open Math.Geometry
open Math.Units

open Utilities.Extensions

[<CustomEquality>]
[<CustomComparison>]
type Edge =
    { Crease: LineSegment2D<Meters, OrigamiCoordinates>
      Assignment: EdgeAssignment }

    (* Accessors *)

    member this.line = Line2D.through this.Crease.Start this.Crease.Finish


    (* Interfaces *)

    interface IComparable<Edge> with
        member this.CompareTo(edge) = this.Comparison(edge)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Edge as edge -> this.Comparison(edge)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other: Edge) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other) =
        if this.Crease = other.Crease then
            this.Assignment < other.Assignment
        else
            this.Crease < other.Crease

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Edge as other -> this.Crease = other.Crease && this.Assignment = other.Assignment
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.Crease, this.Assignment)

module Edge =

    // ---- Builders -----------------------------------------------------------


    /// Create an edge at a particular line segment location and give it an
    /// edge assignment.
    let atWithAssignment crease assignment =
        { Crease = crease
          Assignment = assignment }

    /// Create an edge between two points and give it and edge assignment.
    let betweenWithAssignment start finish assignment =
        atWithAssignment (LineSegment2D.from start finish) assignment


    // ---- Modifiers ----------------------------------------------------------


    /// Scale up the creases edge in both the X and Y axis
    let scale (x: float) edge = { edge with Crease = edge.Crease * x }

    /// Get the minimum distance between a point and the edge.
    /// This is either the perpendicular distance to the line or the distance
    /// to one of the endpoints.
    let distanceToVertex vertex edge =
        edge.Crease |> LineSegment2D.distanceToPoint vertex

    let round edge =
        { edge with
            Crease = LineSegment2D.round edge.Crease }


    // ---- Accessors ---------------------------------------------------------


    /// Get the two vertices that make up the edge.
    let vertices (edge: Edge) = edge.Crease.Start, edge.Crease.Finish

    /// Get the two vertices that make up the edge but as a sequence of two
    /// elements.
    let seqVertices (edges: Edge seq) =
        Seq.map vertices edges |> Seq.map Tuple2.toList |> Seq.concat

    /// Get the line segment that makes up the edge
    let line (e: Edge) : Line2D<Meters, OrigamiCoordinates> = e.line

    /// Get the edge assignment of the edge
    let assignment (e: Edge) : EdgeAssignment = e.Assignment
