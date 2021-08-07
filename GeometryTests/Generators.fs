module GeometryTests.Generators

open FsCheck

open Geometry
open Utilities
open Utilities.Extensions


let vector2D =
    Gen.map2 Vector2D.xy Gen.float Gen.float

let point2D =
    Gen.map2 Point2D.xy Gen.float Gen.float

let line2D =
    Gen.map2 Tuple2.pair point2D point2D
    |> Gen.filter (fun (p1, p2) -> p1 <> p2)
    |> Gen.map (Tuple2.map Line2D.through)

let lineSegment2D =
    Gen.map2 Tuple2.pair point2D point2D
    |> Gen.filter (fun (p1, p2) -> p1 <> p2)
    |> Gen.map (Tuple2.map LineSegment2D.from)

let boundingBox2D =
    Gen.map2 BoundingBox2D.from point2D point2D
