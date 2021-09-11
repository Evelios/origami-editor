namespace CreasePattern

open System
open Geometry
open Utilities.Extensions

[<CustomEquality>]
[<CustomComparison>]
type Edge =
    { crease: LineSegment2D
      assignment: EdgeAssignment }

    (* Accessors *)

    member this.Crease = this.crease

    member this.line =
        Line2D.through this.Crease.Start this.Crease.Finish

    member this.Assignment = this.assignment

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
        if this.crease = other.crease then
            this.assignment < other.assignment
        else
            this.crease < other.crease

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Edge as other ->
            this.crease = other.crease
            && this.assignment = other.assignment
        | _ -> false

    override this.GetHashCode() : int =
        HashCode.Combine(this.crease, this.assignment)

module Edge =

    (* Builders *)

    let atWithAssignment crease assignment =
        { crease = crease
          assignment = assignment }

    let betweenWithAssignment start finish assignment =
        atWithAssignment (LineSegment2D.from start finish) assignment


    (* Modifiers *)

    let scale (x: float) edge = { edge with crease = edge.crease * x }

    let distanceToVertex vertex edge =
        edge.crease
        |> LineSegment2D.distanceToPoint vertex

    let round edge =
        { edge with
              crease = LineSegment2D.round edge.crease }


    (* Accessors *)

    let vertices (edge: Edge) = edge.Crease.Start, edge.Crease.Finish

    let seqVertices (edges: Edge seq) =
        Seq.map vertices edges
        |> Seq.map Tuple2.toList
        |> Seq.concat

    let line (e: Edge) : Line2D = e.line

    let assignment (e: Edge) : EdgeAssignment = e.assignment
