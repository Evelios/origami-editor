namespace CreasePatternTests

module Gen =

    open System
    open FsCheck
    open Math.Geometry
    open Math.Units
    open Math.Units.Test

    open Utilities
    open Utilities.Extensions
    open Origami

    let angle: Gen<Angle> =
        Gen.floatBetween 0. (Math.PI / 2.)
        |> Gen.map Angle.radians

    let quantity<'Units> : Gen<Quantity<'Units>> =
        Gen.float |> Gen.map Quantity.create

    let quantityBetween (low: Quantity<'Units>) (high: Quantity<'Units>) : Gen<Quantity<'Units>> =
        Gen.floatBetween low.Value high.Value
        |> Gen.map Quantity.create

    let vector2D<'Units, 'Coordinates> : Gen<Vector2D<'Units, 'Coordinates>> =
        Gen.map2 Vector2D.xy quantity quantity

    let vector2DWithinRadius (radius: Quantity<'Units>) : Gen<Vector2D<'Units, 'Coordinates>> =
        Gen.map2 Vector2D.polar (quantityBetween Quantity.zero radius) angle

    let twoCloseVector2D<'Units, 'Coordinates> : Gen<Vector2D<'Units, 'Coordinates> * Vector2D<'Units, 'Coordinates>> =
        let create (first: Vector2D<'Units, 'Coordinates>) (offset: Vector2D<'Units, 'Coordinates>) =
            first, first + offset

        Gen.map2 create vector2D (vector2DWithinRadius (Quantity.create Float.Epsilon))

    let point2D<'Units, 'Coordinates> : Gen<Point2D<'Units, 'Coordinates>> =
        Gen.map2 Point2D.xy quantity quantity

    let point2DWithinOffset radius (point: Point2D<'Units, 'Coordinates>) : Gen<Point2D<'Units, 'Coordinates>> =
        Gen.map (fun offset -> point + offset) (vector2DWithinRadius radius)

    /// Generate two points that are within Epsilon of each other
    let twoClosePoint2D<'Units, 'Coordinates> : Gen<Point2D<'Units, 'Coordinates> * Point2D<'Units, 'Coordinates>> =
        let create (first: Point2D<'Units, 'Coordinates>) (offset: Vector2D<'Units, 'Coordinates>) =
            first, first + offset

        Gen.map2 create point2D (vector2DWithinRadius (Quantity.create Float.Epsilon))

    let line2D<'Units, 'Coordinates> : Gen<Line2D<'Units, 'Coordinates>> =
        Gen.map2 Tuple2.pair point2D point2D
        |> Gen.filter (fun (p1, p2) -> p1 <> p2)
        |> Gen.map (Tuple2.map Line2D.through)

    let lineSegment2D<'Units, 'Coordinates> : Gen<LineSegment2D<'Units, 'Coordinates>> =
        Gen.map2 Tuple2.pair point2D point2D
        |> Gen.filter (fun (p1, p2) -> p1 <> p2)
        |> Gen.map (Tuple2.map LineSegment2D.from)

    let boundingBox2D<'Units, 'Coordinates> : Gen<BoundingBox2D<'Units, 'Coordinates>> =
        Gen.map2 BoundingBox2D.from point2D point2D

    let point2DInBoundingBox2D (bbox: BoundingBox2D<'Units, 'Coordinates>) =
        Gen.map2 Point2D.xy (quantityBetween bbox.MinX bbox.MaxX) (quantityBetween bbox.MinY bbox.MaxY)

    let lineSegment2DInBoundingBox2D (bbox: BoundingBox2D<'Units, 'Coordinates>) =
        Gen.two (point2DInBoundingBox2D bbox)
        |> Gen.where (fun (a, b) -> a <> b)
        |> Gen.map (Tuple2.map LineSegment2D.from)

    let edgeAssignment =
        Gen.ofType<EdgeAssignment>

    let creasePattern: Gen<CreasePattern<TestSpace>> =
        let boundingBoxGen = boundingBox2D

        let lineInBoundingBox =
            GenBuilder.gen.Bind(boundingBoxGen, lineSegment2DInBoundingBox2D)

        let creaseInBoundingBox =
            Gen.map2 Edge.atWithAssignment lineInBoundingBox edgeAssignment

        Gen.map2
            (fun boundingBox creases ->
                CreasePattern.withBoundingBox boundingBox
                |> CreasePattern.addEdges creases)
            boundingBoxGen
            (Gen.listOf creaseInBoundingBox)

    type ArbOrigami =
        static member CreasePattern() = Arb.fromGen creasePattern
        static member Vector2D() = Arb.fromGen vector2D
        static member Point2D() = Arb.fromGen point2D
        static member Line2D() = Arb.fromGen line2D
        static member LineSegment2D() = Arb.fromGen line2D
        static member BoundingBox2D() = Arb.fromGen line2D

        static member Register() = Arb.register<ArbOrigami> () |> ignore
