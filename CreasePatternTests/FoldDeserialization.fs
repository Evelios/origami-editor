module CreasePatternTests.FoldDeserialization


open NUnit.Framework
open Fold
open CreasePattern
open Geometry

[<SetUp>]
let SetUp () = ()

[<Test>]
let units () =
    let given =
        Fold.Frame.empty
        |> Fold.Frame.setUnit LengthUnit.Inches

    let expected =
        Frame.empty |> Frame.setUnit LengthUnit.Inches

    Assert.AreEqual(expected, Frame.fromFoldFrame given)


[<Test>]
let creasePattern () =
    /// Given
    let vertices =
        Vertices.create
            { coords =
                  [ Vertex.in2d 0. 0.
                    Vertex.in2d 0. 1.
                    Vertex.in2d 1. 1.
                    Vertex.in2d 1. 0. ]
              vertices = []
              faces = [] }

    let edges =
        Edges.create
            { vertices = [ (0, 1); (1, 2); (2, 3); (3, 0) ]
              faces = []
              assignment =
                  [ Boundary
                    Boundary
                    Boundary
                    Boundary ]
              foldAngle = []
              length = []
              orders = [] }

    let faces =
        Faces.create
            { vertices = [ [ 0; 1; 2; 3 ] ]
              edges = [ [ 0; 1; 2; 3 ] ]
              orders = [] }

    /// Expect

    let v =
        {| bl = Vertex.in2d 0. 0.
           br = Vertex.in2d 0. 1.
           tr = Vertex.in2d 1. 1.
           tl = Vertex.in2d 1. 0. |}

    let expected =
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

    Assert.AreEqual(expected, CreasePattern.fromFoldValues vertices edges faces)
