module CreasePatternTests.FoldSerialization


open NUnit.Framework
open Fold
open CreasePattern

[<SetUp>]
let SetUp () = ()

[<Test>]
let basic () =
    // Given
    let v =
        {| bl = Vertex.in2d 0. 0.
           br = Vertex.in2d 0. 1.
           tr = Vertex.in2d 1. 1.
           tl = Vertex.in2d 1. 0. |}

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
                  [ Vertex.in2d 0. 0.
                    Vertex.in2d 0. 1.
                    Vertex.in2d 1. 0.
                    Vertex.in2d 1. 1. ]
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
