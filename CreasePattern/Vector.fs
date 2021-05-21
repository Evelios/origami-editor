namespace CreasePattern

open System
open MathNet.Spatial.Euclidean


[<CustomEquality>]
[<CustomComparison>]
type Vector =
    | Vec2 of Vector2D
    | Vec3 of Vector3D

    interface IComparable with
        member this.CompareTo(obj) = if this.Equals(obj) then 0 else -1

    interface IComparable<Vector> with
        member this.CompareTo(vector) = if this.Equals(vector) then 0 else -1

    member this.EPSILON = 1e-6
    member this.withinDelta a b = abs (a - b) < this.EPSILON

    override this.Equals(obj: obj) : bool =
        match obj with
        | :? Vector as other ->
            match (this, other) with
            | Vec2 first, Vec2 second ->
                this.withinDelta first.X second.X
                && this.withinDelta first.Y second.Y
            | Vec3 first, Vec3 second ->
                this.withinDelta first.X second.X
                && this.withinDelta first.Y second.Y
                && this.withinDelta first.Z second.Z
            | _ -> false
        | _ -> false

    override this.GetHashCode() = failwith "not implemented"

module Vector =
    let in2D x y : Vector = Vec2(Vector2D(x, y))
    let in3D x y z : Vector = Vec3(Vector3D(x, y, z))
