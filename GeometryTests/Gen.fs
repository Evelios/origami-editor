namespace GeometryTests

module Gen =
    open FsCheck

    open Geometry
    open Utilities
    open Utilities.Extensions


    let vector2D = Gen.map2 Vector2D.xy Gen.float Gen.float

    let point2D = Gen.map2 Point2D.xy Gen.float Gen.float

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

    let point2DInBoundingBox2D (bbox: BoundingBox2D) =
        Gen.map2 Point2D.xy (Gen.floatBetween bbox.minX bbox.maxX) (Gen.floatBetween bbox.minY bbox.maxY)

    let lineSegment2DInBoundingBox2D (bbox: BoundingBox2D) =
        Gen.two (point2DInBoundingBox2D bbox)
        |> Gen.where (fun (a, b) -> a <> b)
        |> Gen.map (Tuple2.map LineSegment2D.from)

    type ArbGeometry =
        static member Vector2D() = Arb.fromGen vector2D
        static member Point2D() = Arb.fromGen point2D
        static member Line2D() = Arb.fromGen line2D
        static member LineSegment2D() = Arb.fromGen line2D
        static member BoundingBox2D() = Arb.fromGen line2D

        static member Register() = Arb.register<ArbGeometry> () |> ignore
