namespace CreasePattern

open System
open Fold
open Geometry

[<CustomEquality>]
[<CustomComparison>]
type Edge =
    { start: Vertex
      finish: Vertex
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
        Line.in2d this.start this.finish < Line.in2d other.start other.finish

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Edge as other ->
            Line.in2d this.start this.finish = Line.in2d other.start other.finish
            && this.assignment = other.assignment
        | _ -> false

    override this.GetHashCode() : int = failwith "not implemented"


module Edge =
    let create a : Edge = a


    let private toLine edge = Line.in2d edge.start edge.finish

    let scale x y z edge =
        { start = Vertex.scale x y z edge.start
          finish = Vertex.scale y y z edge.finish
          assignment = edge.assignment }

    let distanceToVertex vertex edge =
        toLine edge |> Line.distanceToVertex vertex
