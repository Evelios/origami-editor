module CreasePatternTests.FoldSerialization


open NUnit.Framework
open Fold
open CreasePattern
open Geometry

[<SetUp>]
let SetUp () = ()

[<Test>]
let basic () =
    // Given
    let v =
        {| bl = Point2D.xy 0. 0.
           br = Point2D.xy 0. 1.
           tr = Point2D.xy 1. 1.
           tl = Point2D.xy 1. 0. |}

    let creasePattern =
        CreasePattern.empty
        |> CreasePattern.addEdges [ Edge.betweenWithAssignment v.bl v.br Boundary
                                    Edge.betweenWithAssignment v.br v.tr Boundary
                                    Edge.betweenWithAssignment v.tr v.tl Boundary
                                    Edge.betweenWithAssignment v.tl v.bl Boundary ]

    /// Expect
    let vertices =
        Vertices.create
            { coords =
                  [ Point2D.xy 0. 0.
                    Point2D.xy 0. 1.
                    Point2D.xy 1. 0.
                    Point2D.xy 1. 1. ]
              vertices = []
              faces = [] }

    let edges =
        Edges.create
            { vertices = [ (3, 2); (3, 1); (2, 0); (1, 0) ]
              faces = []
              assignment =
                  [ Fold.Boundary
                    Fold.Boundary
                    Fold.Boundary
                    Fold.Boundary ]
              foldAngle = []
              length = []
              orders = [] }

    let expected =
        Frame.empty
        |> Frame.setVertices vertices
        |> Frame.setEdges edges


    /// When
    let actual = CreasePattern.toFoldFrame creasePattern

    Assert.AreEqual(expected, actual)
