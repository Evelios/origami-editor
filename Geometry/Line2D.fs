namespace Geometry

open System
open MathNet.Spatial

[<CustomEquality>]
[<CustomComparison>]
type Line2D =
    | Line2D of Euclidean.Line2D

    (* Comparable interfaces *)

    interface IComparable<Line2D> with
        member this.CompareTo(point) = this.Comparison(point)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Line2D as point -> this.Comparison(point)
            | _ -> failwith "incompatible comparison"

    member this.withinDelta a b = abs (a - b) < Generics.Epsilon

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Line2D as other ->
            match (this, other) with
            | Line2D first, Line2D second -> first.Equals(second)
        | _ -> false


    member this.LessThan(Line2D second) =
        match this with
        | Line2D first ->
            if this.withinDelta first.Direction.X second.Direction.X then
                first.Direction.Y < second.Direction.Y
            else
                first.Direction.X < second.Direction.X

    override this.GetHashCode() =
        match this with
        | Line2D _ -> HashCode.Combine(this.startPoint, this.endPoint)

    (* Accessors *)

    member this.startPoint =
        match this with
        | Line2D line -> Point2D.fromPoint2d line.StartPoint

    member this.endPoint =
        match this with
        | Line2D line -> Point2D.fromPoint2d line.EndPoint


module Line2D =

    (* Builders *)

    /// Does not perform the check that the start point and endpoint are the same point
    let unsafeFromTo (Point2D start) (Point2D finish) =
        Euclidean.Line2D(start, finish) |> Line2D

    /// Checks that the start and endpoint are not the same point
    let fromTo (start: Point2D) (finish: Point2D) =
        if start = finish then
            None
        else
            Some(unsafeFromTo start finish)

    (* Queries *)

    let pointOnLine (Point2D point) (Line2D line) =
        line.ClosestPointTo(point, false)
        |> Point2D.fromPoint2d

    let perpThroughPoint (Point2D euclidPoint: Point2D as point) (Line2D line: Line2D) =
        let perpPoint =
            euclidPoint
            + line.Direction.Rotate(Units.Angle.FromDegrees(90.))
            |> Point2D.fromPoint2d

        unsafeFromTo point perpPoint

    let intersection (Line2D l1) (Line2D l2) : Point2D option =
        Option.ofNullable (l1.IntersectWith(l2))
        |> Option.map Point2D.fromPoint2d


    (* Questions *)

    let isPointOnLine (Point2D point) (Line2D line) =
        point = line.StartPoint
        || point = line.EndPoint
        || line
            .ClosestPointTo(point, false)
            .Equals(point, Generics.Epsilon)


    let isPerpendicularTo (Line2D l1) (Line2D l2) =
        l1.Direction.IsPerpendicularTo(l2.Direction)
