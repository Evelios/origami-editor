namespace Geometry

open System
open FSharp.Json
open MathNet.Spatial

[<CustomEquality>]
[<CustomComparison>]
type Point2D =
    | Point2D of Euclidean.Point2D

    (* Comparable interfaces *)

    interface IComparable<Point2D> with
        member this.CompareTo(point) = this.Comparison(point)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Point2D as point -> this.Comparison(point)
            | _ -> failwith "incompatible comparison"

    member this.withinDelta a b = abs (a - b) < Generics.Epsilon

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Point2D as other ->
            match (this, other) with
            | Point2D first, Point2D second -> first.Equals(second, Generics.Epsilon)
        | _ -> false


    member this.LessThan(Point2D second) =
        match this with
        | Point2D first ->
            if this.withinDelta first.X second.X then
                first.Y < second.Y
            else
                first.X < second.X

    override this.GetHashCode() =
        match this with
        | Point2D point -> HashCode.Combine(point.X, point.Y)

    member this.x =
        match this with
        | Point2D point -> point.X

    member this.y =
        match this with
        | Point2D point -> point.Y

module Point2D =
    (* Builders *)

    let xy x y = Euclidean.Point2D(x, y) |> Point2D

    let internal fromPoint2d = Point2D


    (* Modifiers *)

    let scale x y (Point2D point) =
        Euclidean.Point2D(point.X * x, point.Y * y) |> Point2D

    (* Queries *)

    let distanceSquaredTo (p1: Point2D) (p2: Point2D) : float =
        (p1.x - p2.x) ** 2. + (p1.y - p2.y) ** 2.

    let distanceTo p1 p2 : float = distanceSquaredTo p1 p2 |> sqrt

    (* Json *)
    let fromList (list: float list) : Point2D option =
        match list with
        | [ x; y ] -> Some <| xy x y
        | _ -> None

    let toList point =
        match point with
        | Point2D point -> [ point.X; point.Y ]

    (* Json transformations *)

    type Transform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float32 list>) ()
            member this.toTargetType value = toList (value :?> Point2D) :> obj

            member this.fromTargetType value =
                value :?> float list
                |> fromList
                |> Option.defaultValue (xy 0. 0.)
                :> obj

    type ListTransform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float list list>) ()

            member this.toTargetType value =
                value :?> Point2D list |> List.map toList :> obj

            member this.fromTargetType value =
                value :?> float list list
                |> List.map (fromList >> Option.defaultValue (xy 0. 0.))
                :> obj
