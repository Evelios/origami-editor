namespace Geometry

open System
open FSharp.Json
open MathNet.Spatial

[<CustomEquality>]
[<CustomComparison>]
type Point3 =
    | Point3 of Euclidean.Point3D

    (* Comparable interfaces *)

    interface IComparable<Point3> with
        member this.CompareTo(point) = this.Comparison(point)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Point3 as point -> this.Comparison(point)
            | _ -> failwith "incompatible comparison"

    member this.withinDelta a b = abs (a - b) < Generics.Epsilon

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Point3 as other ->
            match (this, other) with
            | Point3 first, Point3 second -> first.Equals(second, Generics.Epsilon)
        | _ -> false


    member this.LessThan(Point3 second) =
        match this with
        | Point3 first ->
            if this.withinDelta first.X second.X then
                first.Y < second.Y
            else
                first.X < second.X

    override this.GetHashCode() =
        match this with
        | Point3 point -> HashCode.Combine(point.X, point.Y, point.Z)

    member this.x =
        match this with
        | Point3 point -> point.X

    member this.y =
        match this with
        | Point3 point -> point.Y
        
    member this.z =
        match this with
        | Point3 point -> point.Z

module Point3 =
    (* Builders *)

    let xyz x y z = Euclidean.Point3D(x, y, z) |> Point3

    let internal fromPoint3d = Point3

    (* Accessors *)

    let hashCode (Point3 point) = HashCode.Combine(point.X, point.Y, point.Z)


    (* Modifiers *)

    let scale x y z (Point3 point) =
        Euclidean.Point3D(point.X * x, point.Y * y, point.Z * z) |> Point3

    (* Queries *)

    let distanceSquaredTo (p1: Point3) (p2: Point3) : float =
        (p1.x - p2.x) ** 2. + (p1.y - p2.y) ** 2.

    let distanceTo p1 p2 : float = distanceSquaredTo p1 p2 |> sqrt

    (* Json *)
    let fromList (list: float list) : Point3 option =
        match list with
        | [ x; y; z ] -> Some <| xyz x y z
        | _ -> None

    let toList point =
        match point with
        | Point3 point -> [ point.X; point.Y ]

    (* Json transformations *)

    type Transform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float32 list>) ()
            member this.toTargetType value = toList (value :?> Point3) :> obj

            member this.fromTargetType value =
                value :?> float list
                |> fromList
                |> Option.defaultValue (xyz 0. 0. 0.)
                :> obj

    type ListTransform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float list list>) ()

            member this.toTargetType value =
                value :?> Point3 list |> List.map toList :> obj

            member this.fromTargetType value =
                value :?> float list list
                |> List.map (fromList >> Option.defaultValue (xyz 0. 0. 0.))
                :> obj
