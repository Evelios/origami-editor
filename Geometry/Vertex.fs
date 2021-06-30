namespace Geometry

open System
open FSharp.Json
open MathNet.Spatial.Euclidean

[<CustomEquality>]
[<CustomComparison>]
type Vertex =
    | Point2 of Point2D
    | Point3 of Point3D

    member this.Epsilon = 1e-8

    interface IComparable<Vertex> with
        member this.CompareTo(vertex) = this.Comparison(vertex)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Vertex as vertex -> this.Comparison(vertex)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other) =
        match (this, other) with
        | Point2 first, Point2 second ->
            if this.withinDelta first.X second.X then
                first.Y < second.Y
            else
                first.X < second.X
        | Point3 first, Point3 second ->
            if this.withinDelta first.X second.X then
                if this.withinDelta first.Y second.Y then
                    first.Z < second.Z
                else
                    first.Y < second.Y
            else
                first.X < second.X
        | _ -> false


    member this.withinDelta a b = abs (a - b) < this.Epsilon

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Vertex as other ->
            match (this, other) with
            | Point2 first, Point2 second ->
                this.withinDelta first.X second.X
                && this.withinDelta first.Y second.Y
            | Point3 first, Point3 second ->
                this.withinDelta first.X second.X
                && this.withinDelta first.Y second.Y
                && this.withinDelta first.Z second.Z
            | _ -> false
        | _ -> false

    override this.GetHashCode() = failwith "not implemented"

module Vertex =
    [<Literal>]
    let epsilon = 1e-6

    (* Builders *)

    let in2d x y = Point2D(x, y) |> Vertex.Point2

    let in3d x y z = Point3D(x, y, z) |> Vertex.Point3
    
    let internal fromPoint2d = Vertex.Point2
    let internal fromPoint3d = Vertex.Point2


    (* Accessors *)

    let x point =
        match point with
        | Point2 point -> point.X
        | Point3 point -> point.X

    let y point =
        match point with
        | Point2 point -> point.Y
        | Point3 point -> point.Y

    let z point =
        match point with
        | Point2 _ -> 0.
        | Point3 point -> point.Z

    let hashCode point =
        match point with
        | Point2 point -> HashCode.Combine(point.X, point.Y, 0)
        | Point3 point -> HashCode.Combine(point.X, point.Y, point.Z)


    (* Modifiers *)

    let scale x y z point =
        match point with
        | Point2 point -> Point2 <| Point2D(point.X * x, point.Y * y)
        | Point3 point ->
            Point3
            <| Point3D(point.X * x, point.Y * y, point.Z * z)


    (* Queries *)

    let distanceSquaredTo v1 v2 : float =
        (x v1 - x v2) ** 2.
        + (y v1 - y v2) ** 2.
        + (z v1 - z v2) ** 2.

    let distanceTo v1 v2 : float = distanceSquaredTo v1 v2 |> sqrt


    (* Json *)
    let fromList (list: float list) : Vertex option =
        match list with
        | [ x; y ] -> Some <| in2d x y
        | [ x; y; z ] -> Some <| in3d x y z
        | _ -> None

    let toList vertex =
        match vertex with
        | Point2 point -> [ point.X; point.Y ]
        | Point3 point -> [ point.X; point.Y; point.Z ]

    (* Json transformations *)

    type Transform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float32 list>) ()
            member this.toTargetType value = toList (value :?> Vertex) :> obj

            member this.fromTargetType value =
                value :?> float list
                |> fromList
                |> Option.defaultValue (in3d 0. 0. 0.)
                :> obj

    type ListTransform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float list list>) ()

            member this.toTargetType value =
                value :?> Vertex list |> List.map toList :> obj

            member this.fromTargetType value =
                value :?> float list list
                |> List.map (fromList >> Option.defaultValue (in3d 0. 0. 0.))
                :> obj
