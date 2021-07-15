module CreasePatternTests.FoldDeserialization


open NUnit.Framework
open Fold
open CreasePattern
open Geometry

[<SetUp>]
let SetUp () = ()

[<Test>]
let ``Convert metadata`` () =
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
let ``Set graph`` () =
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
                  [ Fold.Boundary
                    Fold.Boundary
                    Fold.Boundary
                    Fold.Boundary ]
              foldAngle = []
              length = []
              orders = [] }

    let faces =
        Faces.create
            { vertices = [ [ 0; 1; 2; 3 ] ]
              edges = [ [ 0; 1; 2; 3 ] ]
              orders = [] }

    let frame =
        Frame.empty
        |> Frame.setVertices vertices
        |> Frame.setEdges edges
        |> Frame.setFaces faces
        

    /// Expect

    let v =
        {| bl = Point2D.xy 0. 0.
           br = Point2D.xy 0. 1.
           tr = Point2D.xy 1. 1.
           tl = Point2D.xy 1. 0. |}

    let expected =
        CreasePattern.empty
        |> CreasePattern.addEdges [ Edge.betweenWithAssignment v.bl v.br Boundary
                                    Edge.betweenWithAssignment v.br v.tr Boundary
                                    Edge.betweenWithAssignment v.tr v.tl Boundary
                                    Edge.betweenWithAssignment v.tl v.bl Boundary ]

    Assert.AreEqual(expected, CreasePattern.fromFoldFrame frame)
