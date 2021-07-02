namespace Geometry

open System
open MathNet.Spatial

[<CustomEquality>]
[<CustomComparison>]
type LineSegment2D =
    | LineSegment2D of Euclidean.LineSegment2D

    interface IComparable<LineSegment2D> with
        member this.CompareTo(line) = this.Comparison(line)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? LineSegment2D as vertex -> this.Comparison(vertex)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other) =
        match (this, other) with
        | LineSegment2D first, LineSegment2D second ->
            let firstLower =
                min (Point2D.fromPoint2d first.StartPoint) (Point2D.fromPoint2d first.EndPoint)

            let firstGreater =
                min (Point2D.fromPoint2d first.StartPoint) (Point2D.fromPoint2d first.EndPoint)

            let secondLower =
                min (Point2D.fromPoint2d second.StartPoint) (Point2D.fromPoint2d second.EndPoint)

            let secondGreater =
                min (Point2D.fromPoint2d second.StartPoint) (Point2D.fromPoint2d second.EndPoint)

            if firstLower = secondLower then
                firstGreater < secondGreater
            else
                firstLower < secondLower

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? LineSegment2D as other ->
            match (this, other) with
            | LineSegment2D first, LineSegment2D second ->
                (first.StartPoint = second.StartPoint
                 && first.EndPoint = second.EndPoint)
                || (first.StartPoint = second.EndPoint
                    && first.EndPoint = second.StartPoint)
        | _ -> false

    override this.GetHashCode() : int = failwith "not implemented"

module LineSegment2D =
    let fromTo (Point2D start) (Point2D finish) =
        Euclidean.LineSegment2D(start, finish) |> LineSegment2D

    let distanceToVertex (Point2D point) (LineSegment2D line) = (line.LineTo point).Length
