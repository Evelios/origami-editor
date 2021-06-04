namespace Fold

open System
open FSharp.Json
open MathNet.Spatial.Euclidean

[<CustomEquality>]
[<CustomComparison>]
type Vertex =
    | Vector2 of Vector2D
    | Vector3 of Vector3D

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
        | Vector2 first, Vector2 second ->
            if this.withinDelta first.X second.X then
                first.Y < second.Y
            else
                first.X < second.X
        | Vector3 first, Vector3 second ->
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
            | Vector2 first, Vector2 second ->
                this.withinDelta first.X second.X
                && this.withinDelta first.Y second.Y
            | Vector3 first, Vector3 second ->
                this.withinDelta first.X second.X
                && this.withinDelta first.Y second.Y
                && this.withinDelta first.Z second.Z
            | _ -> false
        | _ -> false

    override this.GetHashCode() = failwith "not implemented"

module Vertex =
    (* Builders *)

    let in2d x y = Vector2D(x, y) |> Vertex.Vector2

    let in3d x y z = Vector3D(x, y, z) |> Vertex.Vector3


    (* Accessors *)

    let x vector =
        match vector with
        | Vector2 vector -> vector.X
        | Vector3 vector -> vector.X

    let y vector =
        match vector with
        | Vector2 vector -> vector.Y
        | Vector3 vector -> vector.Y

    let hashCode vector =
        match vector with
        | Vector2 vector -> HashCode.Combine(vector.X, vector.Y, 0)
        | Vector3 vector -> HashCode.Combine(vector.X, vector.Y, vector.Z)


    (* Modifiers *)

    let scale x y z vector =
        match vector with
        | Vector2 vector ->
            Vector2
            <| Vector2D(vector.X * x, vector.Y * y)
        | Vector3 vector ->
            Vector3
            <| Vector3D(vector.X * x, vector.Y * y, vector.Z * z)



    (* Json *)
    let fromList (list: float list) : Vertex option =
        match list with
        | [ x; y ] -> Some <| in2d x y
        | [ x; y; z ] -> Some <| in3d x y z
        | _ -> None

    let toList vertex =
        match vertex with
        | Vector2 vec -> [ vec.X; vec.Y ]
        | Vector3 vec -> [ vec.X; vec.Y; vec.Z ]

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
