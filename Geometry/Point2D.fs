namespace Geometry

open System
open FSharp.Json

[<CustomEquality>]
[<CustomComparison>]
[<RequireQualifiedAccess>]
[<Struct>]
type Point2D =
    private
        { x: float
          y: float }

    member this.X = this.x
    member this.Y = this.y

    (* Comparable interfaces *)

    interface IComparable<Point2D> with
        member this.CompareTo(point) = this.Comparison(point)

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Point2D as point -> this.Comparison(point)
            | _ -> failwith "incompatible comparison"

    member this.Comparison(other) =
        if this.Equals(other) then 0
        elif this.LessThan(other) then -1
        else 1

    member this.LessThan(other: Point2D) =
        if almostEqual (float this.x) (float other.x) then
            float this.y < float other.y
        else
            float this.x < float other.x

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Point2D as other -> this.Equals(other)
        | _ -> false

    member this.Equals(other: Point2D) : bool =
        almostEqual this.x other.x
        && almostEqual this.y other.y

    override this.GetHashCode() = HashCode.Combine(this.x, this.y)

    static member (-)(lhs: Point2D, rhs: Point2D) : Vector2D =
        Vector2D.xy (lhs.x - rhs.x) (lhs.y - rhs.y)

    static member (+)(lhs: Point2D, rhs: Vector2D) : Point2D =
        { x = lhs.x + rhs.x; y = lhs.y + rhs.y }

    static member (*)(lhs: Point2D, rhs: float) : Point2D = { x = lhs.x * rhs; y = lhs.y * rhs }

    static member (*)(lhs: float, rhs: Point2D) : Point2D = rhs * lhs

    static member (/)(lhs: Point2D, rhs: float) : Point2D = { x = lhs.x / rhs; y = lhs.y / rhs }

    static member (/)(lhs: float, rhs: Point2D) : Point2D = rhs / lhs


module Point2D =
    (* Builders *)

    let xy (x: float) (y: float) : Point2D = { x = x; y = y }

    let ofPolar r a = xy (r * Angle.cos a) (r * Angle.sin a)

    let origin = xy 0. 0.

    (* Modifiers *)

    let scale x y (point: Point2D) : Point2D = { x = point.x * x; y = point.y * y }

    let translate (v: Vector2D) (p: Point2D) = p + v

    (* Queries *)

    let distanceSquaredTo (p1: Point2D) (p2: Point2D) : float =
        let dx = (p1.x - p2.x)
        let dy = (p1.y - p2.y)
        dx * dx + dy * dy

    let distanceTo p1 p2 : float = distanceSquaredTo p1 p2 |> sqrt

    let midpoint (p1: Point2D) (p2: Point2D) : Point2D =
        xy ((p1.x + p2.x) / 2.) ((p1.y + p2.y) / 2.)

    let round (p: Point2D) = xy (roundFloat p.x) (roundFloat p.y)


    /// Be careful with the vector arguments. This function is written with piping in mind. The first point is the
    /// target location. The second point is the starting location
    let vectorTo (target: Point2D) (from: Point2D) : Vector2D = target - from


    (* Json *)
    let fromList (list: float list) : Point2D option =
        match list with
        | [ x; y ] -> Some <| xy x y
        | _ -> None

    let toList (point: Point2D) : float list = [ point.x; point.y ]

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
