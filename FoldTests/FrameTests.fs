module FoldTests.FrameTests

open NUnit.Framework
open Fold
open Geometry

[<SetUp>]
let Setup () = ()

let testCases =
    [ """{"frame_unit":"unit"}""",
      { Frame.empty with
            unit = LengthUnit.Unitless }
      """{"frame_author":"The Author","frame_unit":"unit"}""",
      { Frame.empty with
            author = "The Author" }

      """{"frame_title":"The Title","frame_unit":"unit"}""", { Frame.empty with title = "The Title" }

      """{"frame_description":"The Description","frame_unit":"unit"}""",
      { Frame.empty with
            description = "The Description" }

      """{"frame_classes":["creasePattern","graph"],"frame_unit":"unit"}""",
      { Frame.empty with
            classes =
                Set.ofList [ FrameClass.CreasePattern
                             FrameClass.Graph ] }

      """{"frame_attributes":["2D","orientable"],"frame_unit":"unit"}""",
      { Frame.empty with
            attributes =
                Set.ofList [ FrameAttribute.Geo2D
                             FrameAttribute.Orientable ] }
      """{"frame_unit":"unit","vertices_coords":[[-1,-1],[1,1]]}""",
      { Frame.empty with
            vertices =
                { Vertices.empty with
                      coords =
                          [ Vertex.in2d -1. -1.
                            Vertex.in2d 1. 1. ] } }
      """{"frame_unit":"unit","vertices_coords":[[0.5,1],[2.5,2.5]]}""",
      { Frame.empty with
            vertices =
                { Vertices.empty with
                      coords =
                          [ Vertex.in2d 0.5 1.
                            Vertex.in2d 2.5 2.5 ] } }
      """{"frame_unit":"unit","vertices_vertices":[0,1,2]}""",
      { Frame.empty with
            vertices =
                { Vertices.empty with
                      vertices = [ 0; 1; 2 ] } }
      """{"frame_unit":"unit","vertices_faces":[[0,1,2]]}""",
      { Frame.empty with
            vertices =
                { Vertices.empty with
                      faces = [ [ 0; 1; 2 ] ] } }
      """{"frame_unit":"unit","edges_vertices":[[1,0],[0,2]]}""",
      { Frame.empty with
            edges =
                { Edges.empty with
                      vertices = [ (1, 0); (0, 2) ] } }
      """{"frame_unit":"unit","edges_faces":[[1,2],[2,3]]}""",
      { Frame.empty with
            edges =
                { Edges.empty with
                      faces = [ (1, Some 2); (2, Some 3) ] } }
      """{"frame_unit":"unit","edges_faces":[[1,null],[2,null]]}""",
      { Frame.empty with
            edges =
                { Edges.empty with
                      faces = [ (1, None); (2, None) ] } }
      """{"frame_unit":"unit","edges_assignment":["B","U"]}""",
      { Frame.empty with
            edges =
                { Edges.empty with
                      assignment =
                          [ EdgeAssignment.Boundary
                            EdgeAssignment.Unassigned ] } }
      """{"frame_unit":"unit","edges_foldAngle":[0.7,0.1]}""",
      { Frame.empty with
            edges =
                { Edges.empty with
                      foldAngle = [ 0.7; 0.1 ] } }
      """{"frame_unit":"unit","edges_length":[10,20]}""",
      { Frame.empty with
            edges =
                { Edges.empty with
                      length = [ 10.; 20. ] } }
      """{"frame_unit":"unit","faces_vertices":[[1,2,3],[3,4,1]]}""",
      { Frame.empty with
            faces =
                { Faces.empty with
                      vertices = [ [ 1; 2; 3 ]; [ 3; 4; 1 ] ] } }
      """{"frame_unit":"unit","faces_edges":[[1,2,3],[3,4,1]]}""",
      { Frame.empty with
            faces =
                { Faces.empty with
                      edges = [ [ 1; 2; 3 ]; [ 3; 4; 1 ] ] } }
      """{"frame_unit":"unit","faceOrders":[[0,2,-1],[1,4,0]]}""",
      { Frame.empty with
            faces =
                { Faces.empty with
                      orders = [ (0, 2, -1); (1, 4, 0) ] } } ]


let deserializationTestCases = Util.toTest testCases

[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = FrameJson.FromJson source


let serializationTestCases = Util.toTestReverse testCases

[<TestCaseSource("serializationTestCases")>]
let Serialization source = FrameJson.ToJsonUnformatted source
