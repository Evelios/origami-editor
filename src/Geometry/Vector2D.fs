namespace Geometry

open System
open FSharp.Json

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
[<Struct>]
type Vector2D =
    private
        { x: float
          y: float }

    member this.X = this.x
    member this.Y = this.y

    (* Comparable interfaces *)

    interface IComparable<Vector2D> with
        member this.CompareTo(vector) = this.Comparison(vector)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Vector2D as vector -> this.Comparison(vector)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Vector2D) =
        if almostEqual (float this.x) (float other.x) then
            float this.y < float other.y
        else
            float this.x < float other.x

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Vector2D as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Vector2D) : bool =
        almostEqual this.x other.x
        && almostEqual this.y other.y

    override this.GetHashCode() = HashCode.Combine(this.x, this.y)

    static member (+)(lhs: Vector2D, rhs: Vector2D) : Vector2D =
        { x = lhs.x + rhs.x; y = lhs.y + rhs.y }

    static member (-)(lhs: Vector2D, rhs: Vector2D) : Vector2D =
        { x = lhs.x + rhs.x; y = lhs.y + rhs.y }

    static member (*)(vector: Vector2D, scale: float) : Vector2D =
        { x = vector.x * scale
          y = vector.y * scale }

    static member (*)(scale: float, vector: Vector2D) : Vector2D = vector * scale

    static member (/)(vector: Vector2D, scale: float) : Vector2D =
        { x = vector.x / scale
          y = vector.y / scale }

    static member (/)(scale: float, vector: Vector2D) : Vector2D = vector / scale

module Vector2D =
    (* Builders *)

    let xy (x: float) (y: float) : Vector2D = { x = x; y = y }

    let ofPolar r a = xy (r * Angle.cos a) (r * Angle.sin a)

    (* Accessors *)

    let magnitude (v: Vector2D) = sqrt (v.x ** 2. + v.y ** 2.)


    (* Modifiers *)

    let scale x y (vector: Vector2D) : Vector2D = { x = vector.x * x; y = vector.y * y }

    let mul scale (v: Vector2D) = v * scale

    let neg (v: Vector2D) : Vector2D = { x = -v.x; y = -v.y }

    let rotate a (v: Vector2D) : Vector2D =
        { x = Angle.cos a * v.x - Angle.sin a * v.y
          y = Angle.sin a * v.x + Angle.cos a * v.y }

    let normalize v = v / (magnitude v)

    let round (p: Vector2D) = xy (roundFloat p.x) (roundFloat p.y)


    (* Queries *)

    let distanceSquaredTo (p1: Vector2D) (p2: Vector2D) : float =
        let dx = (p1.x - p2.x)
        let dy = (p1.y - p2.y)
        dx * dx + dy * dy

    let distanceTo p1 p2 : float = distanceSquaredTo p1 p2 |> sqrt

    let midVector (p1: Vector2D) (p2: Vector2D) : Vector2D =
        xy ((p1.x + p2.x) / 2.) ((p1.y + p2.y) / 2.)

    let dotProduct (lhs: Vector2D) (rhs: Vector2D) : float = (lhs.x * rhs.x) + (lhs.y * rhs.y)

    let crossProduct (lhs: Vector2D) (rhs: Vector2D) : float = (lhs.x * rhs.y) - (lhs.y * rhs.x)


    (* Json *)

    let fromList (list: float list) : Vector2D option =
        match list with
        | [ x; y ] -> Some <| xy x y
        | _ -> None

    let toList (vector: Vector2D) : float list = [ vector.x; vector.y ]


    (* Json transformations *)

    type Transform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float32 list>) ()
            member this.toTargetType value = toList (value :?> Vector2D) :> obj

            member this.fromTargetType value =
                value :?> float list
                |> fromList
                |> Option.defaultValue (xy 0. 0.)
                :> obj

    type ListTransform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float list list>) ()

            member this.toTargetType value =
                value :?> Vector2D list |> List.map toList :> obj

            member this.fromTargetType value =
                value :?> float list list
                |> List.map (fromList >> Option.defaultValue (xy 0. 0.))
                :> obj
