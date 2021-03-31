namespace Fold

open System.Numerics
open FSharp.Json

type Vertex =
    | Vector2 of Vector2
    | Vector3 of Vector3

module Vertex =
    let Precision: int = 8

    let in2d x y = new Vector2(x, y) |> Vertex.Vector2
    let in3d x y z = new Vector3(x, y, z) |> Vertex.Vector3

    (* Json *)
    let fromList (list: float32 list): Vertex option =
        match list with
        | [ x; y ] -> Some <| in2d  x y
        | [ x; y; z ] -> Some <| in3d x y z
        | _ -> None

    let toList vertex =
        match vertex with
        | Vector2 vec -> [vec.X; vec.Y]
        | Vector3 vec -> [vec.X; vec.Y; vec.Z]

    type Transform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float32 list>) ()
            member this.toTargetType value = toList(value :?> Vertex) :> obj

            member this.fromTargetType value =
                value :?> float32 list
                |> fromList
                |> Option.defaultValue (in3d 0.f 0.f 0.f) :> obj

    type ListTransform() =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<float32 list list>) ()
            member this.toTargetType value =
                value :?> Vertex list
                |> List.map toList
                :> obj

            member this.fromTargetType value =
                value :?> float32 list list
                |> List.map (fromList >> Option.defaultValue (in3d 0.f 0.f 0.f))
                :> obj
