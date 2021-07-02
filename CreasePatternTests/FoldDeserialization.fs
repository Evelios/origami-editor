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
                  [ Point2D.xy 0. 0.
                    Point2D.xy 0. 1.
                    Point2D.xy 1. 1.
                    Point2D.xy 1. 0. ]
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
        {| bl = Point2D.xy 0. 0.
           br = Point2D.xy 0. 1.
           tr = Point2D.xy 1. 1.
           tl = Point2D.xy 1. 0. |}

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
