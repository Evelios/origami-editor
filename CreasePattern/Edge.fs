namespace CreasePattern

open System
open Fold
open Geometry

[<CustomEquality>]
[<CustomComparison>]
type Edge =
    | Edge of
        {| line: LineSegment2D
           assignment: EdgeAssignment |}

    (* Accessors *)

    member this.crease =
        match this with
        | Edge edge -> edge.line

    member this.line =
        match this with
        | Edge edge -> Line2D.fromTo edge.line.start edge.line.finish


    member this.assignment =
        match this with
        | Edge edge -> edge.assignment

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
        LineSegment2D.from this.crease.start this.crease.finish < LineSegment2D.from
                                                                        other.crease.start
                                                                        other.crease.finish

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Edge as other ->
            LineSegment2D.from this.crease.start this.crease.finish = LineSegment2D.from
                                                                            other.crease.start
                                                                            other.crease.finish
            && this.assignment = other.assignment
        | _ -> false

    override this.GetHashCode() : int = failwith "not implemented"

module Edge =

    (* Builders *)

    let atWithAssignment line assignment =
        Edge
            {| line = line
               assignment = assignment |}

    let betweenWithAssignment start finish assignment =
        atWithAssignment (LineSegment2D.from start finish) assignment

    let scale (x: float) (Edge edge: Edge) =
        Edge {| edge with line = edge.line * x |}

    let distanceToVertex vertex (Edge edge) =
        edge.line |> LineSegment2D.distanceToPoint vertex
