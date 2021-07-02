namespace CreasePattern

open System
open Fold
open Geometry

[<CustomEquality>]
[<CustomComparison>]
type Edge =
    { start: Point2D
      finish: Point2D
      assignment: EdgeAssignment }

    interface IComparable<Edge> with
        member this.CompareTo(line) = this.Comparison(line)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Edge as vertex -> this.Comparison(vertex)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other) =
        LineSegment2D.fromTo this.start this.finish < LineSegment2D.fromTo other.start other.finish

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Edge as other ->
            LineSegment2D.fromTo this.start this.finish = LineSegment2D.fromTo other.start other.finish
            && this.assignment = other.assignment
        | _ -> false

    override this.GetHashCode() : int = failwith "not implemented"


    (* Accessors *)

    member this.line =
        Line2D.unsafeFromTo this.start this.finish

module Edge =
    let create a : Edge = a


    let private toLine edge =
        LineSegment2D.fromTo edge.start edge.finish

    let scale x y edge =
        { start = Point2D.scale x y edge.start
          finish = Point2D.scale y y edge.finish
          assignment = edge.assignment }

    let distanceToVertex vertex edge =
        toLine edge
        |> LineSegment2D.distanceToVertex vertex
