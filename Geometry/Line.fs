namespace Geometry

open System
open MathNet.Spatial.Euclidean

[<CustomEquality>]
[<CustomComparison>]
type Line =
    | Line2 of LineSegment2D

    interface IComparable<Line> with
        member this.CompareTo(line) = this.Comparison(line)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Line as vertex -> this.Comparison(vertex)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other) =
        match (this, other) with
        | Line2 first, Line2 second ->
            let firstLower =
                min (Vertex.fromPoint2d first.StartPoint) (Vertex.fromPoint2d first.EndPoint)

            let firstGreater =
                min (Vertex.fromPoint2d first.StartPoint) (Vertex.fromPoint2d first.EndPoint)

            let secondLower =
                min (Vertex.fromPoint2d second.StartPoint) (Vertex.fromPoint2d second.EndPoint)

            let secondGreater =
                min (Vertex.fromPoint2d second.StartPoint) (Vertex.fromPoint2d second.EndPoint)

            if firstLower = secondLower then
                firstGreater < secondGreater
            else
                firstLower < secondLower

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Line as other ->
            match (this, other) with
            | Line2 first, Line2 second ->
                (first.StartPoint = second.StartPoint
                 && first.EndPoint = second.EndPoint)
                || (first.StartPoint = second.EndPoint
                    && first.EndPoint = second.StartPoint)
        | _ -> false

    override this.GetHashCode() : int = failwith "not implemented"

module Line =
    let in2d start finish =
        match start, finish with
        | Point2 start2d, Point2 finish2d -> Line2 <| LineSegment2D(start2d, finish2d)
        | _ -> failwith "Can only be used on 2D vertexes"

    let distanceToVertex (vertex: Vertex) (Line2 line) =
        match vertex with
        | Point2 point -> (line.LineTo point).Length

        | Point3 _ -> failwith "Can't get the distance to a 3D vertex"
