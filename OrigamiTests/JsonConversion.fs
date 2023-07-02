module CreasePatternTests.FoldDeserialization


open NUnit.Framework
open FsCheck

open Fold
open Origami
open Math.Geometry
open Utilities

[<SetUp>]
let SetUp () = Gen.ArbOrigami.Register()

[<Test>]
let ``Basic Serialization`` () =
    // Given
    let v =
        {| bl = Point2D.meters 0. 0.
           br = Point2D.meters 0. 1.
           tr = Point2D.meters 1. 1.
           tl = Point2D.meters 1. 0. |}

    let creasePattern =
        CreasePattern.empty
        |> CreasePattern.addEdges [ Edge.betweenWithAssignment v.bl v.br EdgeAssignment.Boundary
                                    Edge.betweenWithAssignment v.br v.tr EdgeAssignment.Boundary
                                    Edge.betweenWithAssignment v.tr v.tl EdgeAssignment.Boundary
                                    Edge.betweenWithAssignment v.tl v.bl EdgeAssignment.Boundary ]

    /// Expect
    let vertices =
        Vertices.create
            { Coordinates =
                  [ Point2D.meters 0. 0.
                    Point2D.meters 0. 1.
                    Point2D.meters 1. 0.
                    Point2D.meters 1. 1. ]
              Vertices = []
              Faces = [] }

    let edges =
        Edges.create
            { Vertices = [ (0, 1); (2, 0); (1, 3); (3, 2) ]
              Faces = []
              Assignment =
                  [ Fold.EdgeAssignment.Boundary
                    Fold.EdgeAssignment.Boundary
                    Fold.EdgeAssignment.Boundary
                    Fold.EdgeAssignment.Boundary ]
              FoldAngle = []
              Length = []
              Orders = [] }

    let expected =
        Frame.empty
        |> Frame.setVertices vertices
        |> Frame.setEdges edges


    /// When
    let actual = CreasePattern.toFoldFrame creasePattern

    Assert.AreEqual(expected, actual)

[<Test>]
let ``Deserialize metadata`` () =
    let given =
        Frame.empty
        |> Frame.setUnit Fold.LengthUnit.Meters
        |> Frame.setAuthor "The Author"
        |> Frame.setTitle "The Title"
        |> Frame.setDescription "The description"

    let expected =
        CreasePattern.empty
        |> CreasePattern.setUnit LengthUnit.Meters
        |> CreasePattern.setAuthor "The Author"
        |> CreasePattern.setTitle "The Title"
        |> CreasePattern.setDescription "The description"

    Assert.AreEqual(expected, CreasePattern.fromFoldFrame given)

[<Test>]
let ``Deserialize edges`` () =
    /// Given
    let vertices =
        Vertices.create
            { Coordinates =
                  [ Point2D.meters 0. 0.
                    Point2D.meters 0. 1.
                    Point2D.meters 1. 1.
                    Point2D.meters 1. 0. ]
              Vertices = []
              Faces = [] }

    let edges =
        Edges.create
            { Vertices = [ (0, 1); (1, 2); (2, 3); (3, 0) ]
              Faces = []
              Assignment =
                  [ Fold.EdgeAssignment.Boundary
                    Fold.EdgeAssignment.Boundary
                    Fold.EdgeAssignment.Boundary
                    Fold.EdgeAssignment.Boundary ]
              FoldAngle = []
              Length = []
              Orders = [] }

    let faces =
        Faces.create
            { Vertices = [ [ 0; 1; 2; 3 ] ]
              Edges = [ [ 0; 1; 2; 3 ] ]
              Orders = [] }

    let frame =
        Frame.empty
        |> Frame.setVertices vertices
        |> Frame.setEdges edges
        |> Frame.setFaces faces


    /// Expect

    let v =
        {| bl = Point2D.meters 0. 0.
           br = Point2D.meters 0. 1.
           tr = Point2D.meters 1. 1.
           tl = Point2D.meters 1. 0. |}

    let expected =
        CreasePattern.empty
        |> CreasePattern.addEdges [ Edge.betweenWithAssignment v.bl v.br EdgeAssignment.Boundary
                                    Edge.betweenWithAssignment v.br v.tr EdgeAssignment.Boundary
                                    Edge.betweenWithAssignment v.tr v.tl EdgeAssignment.Boundary
                                    Edge.betweenWithAssignment v.tl v.bl EdgeAssignment.Boundary ]

    Assert.AreEqual(expected, CreasePattern.fromFoldFrame frame)


// TODO: Fix serialization and deserialization differences between points that are close
//[<Property>]
//let ``Serialize & Deserialize`` creasePattern =
//    let deserialized =
//        creasePattern
//        |> CreasePattern.toJson
//        |> CreasePattern.fromJson
//
//    creasePattern .=. deserialized
