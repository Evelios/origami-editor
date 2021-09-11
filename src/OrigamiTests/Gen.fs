namespace CreasePatternTests

module Gen =

    open FsCheck


    open Utilities
    open CreasePattern
    open GeometryTests


    let edgeAssignment = Gen.ofType<EdgeAssignment>

    let creasePattern =
        let boundingBoxGen = Gen.boundingBox2D

        let lineInBoundingBox =
            GenBuilder.gen.Bind(boundingBoxGen, Gen.lineSegment2DInBoundingBox2D)

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

        static member Register() = Arb.register<ArbOrigami> () |> ignore
