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
        |> CreasePattern.addEdges [ Edge.create
                                        { start = v.bl
                                          finish = v.br
                                          assignment = Boundary }
                                    Edge.create
                                        { start = v.br
                                          finish = v.tr
                                          assignment = Boundary }
                                    Edge.create
                                        { start = v.tr
                                          finish = v.tl
                                          assignment = Boundary }
                                    Edge.create
                                        { start = v.tl
                                          finish = v.bl
                                          assignment = Boundary } ]

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
            { vertices = [ (2, 3); (1, 3); (0, 2); (0, 1) ]
              faces = []
              assignment =
                  [ Boundary
                    Boundary
                    Boundary
                    Boundary ]
              foldAngle = []
              length = []
              orders = [] }

    let expected =
        Fold.Frame.empty
        |> Fold.Frame.setVertices vertices
        |> Fold.Frame.setEdges edges


    /// When
    let actual =
        Fold.Frame.empty
        |> CreasePattern.addToFoldFrame creasePattern

    Assert.AreEqual(expected, actual)
