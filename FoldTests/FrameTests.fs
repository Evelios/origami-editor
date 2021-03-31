module FoldTests.FrameTests

open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()

let testCases =
    [ """{"frame_unit":"unit"}""",
      { Frame.Empty with
            unit = Unit.Unitless }
      """{"frame_author":"The Author","frame_unit":"unit"}""",
      { Frame.Empty with
            author = "The Author" }

      """{"frame_title":"The Title","frame_unit":"unit"}""", { Frame.Empty with title = "The Title" }

      """{"frame_description":"The Description","frame_unit":"unit"}""",
      { Frame.Empty with
            description = "The Description" }

      """{"frame_classes":["creasePattern","graph"],"frame_unit":"unit"}""",
      { Frame.Empty with
            classes =
                Set.ofList [ FrameClass.CreasePattern
                             FrameClass.Graph ] }

      """{"frame_attributes":["2D","orientable"],"frame_unit":"unit"}""",
      { Frame.Empty with
            attributes =
                Set.ofList [ FrameAttribute.Geo2D
                             FrameAttribute.Orientable ] }
      """{"frame_unit":"unit","vertices_coords":[[-1,-1],[1,1]]}""",
      { Frame.Empty with
            vertices =
                { Vertices.Empty with
                      coords =
                          Some [ Vertex.in2d -1.f -1.f
                                 Vertex.in2d 1.f 1.f ] } }
      """{"frame_unit":"unit","vertices_coords":[[0.5,1],[2.5,2.5]]}""",
      { Frame.Empty with
            vertices =
                { Vertices.Empty with
                      coords =
                          Some [ Vertex.in2d 0.5f 1.f
                                 Vertex.in2d 2.5f 2.5f ] } }
      """{"frame_unit":"unit","vertices_vertices":[0,1,2]}""",
      { Frame.Empty with
            vertices =
                { Vertices.Empty with
                      vertices = Some [ 0; 1; 2 ] } }
      """{"frame_unit":"unit","vertices_faces":[[0,1,2]]}""",
      { Frame.Empty with
            vertices =
                { Vertices.Empty with
                      faces = Some [ [ 0; 1; 2 ] ] } }
      """{"frame_unit":"unit","edges_vertices":[[1,0],[0,2]]}""",
      { Frame.Empty with
            edges =
                { Edges.Empty with
                      vertices = Some [ (1, 0); (0, 2) ] } }
      """{"frame_unit":"unit","edges_faces":[[1,2],[2,3]]}""",
      { Frame.Empty with
            edges =
                { Edges.Empty with
                      faces = Some [ (1, Some 2); (2, Some 3) ] } }
      """{"frame_unit":"unit","edges_faces":[[1,null],[2,null]]}""",
      { Frame.Empty with
            edges =
                { Edges.Empty with
                      faces = Some [ (1, None); (2, None) ] } }
      """{"frame_unit":"unit","edges_assignment":["B","U"]}""",
      { Frame.Empty with
            edges =
                { Edges.Empty with
                      assignment =
                          Some [ EdgeAssignment.Boundary
                                 EdgeAssignment.Unassigned ] } }
      """{"frame_unit":"unit","edges_foldAngle":[0.7,0.1]}""",
      { Frame.Empty with
            edges =
                { Edges.Empty with
                      foldAngle = Some [ 0.7; 0.1 ] } }
      """{"frame_unit":"unit","edges_length":[10,20]}""",
      { Frame.Empty with
            edges =
                { Edges.Empty with
                      length = Some [ 10.; 20. ] } }
      """{"frame_unit":"unit","faces_vertices":[[1,2,3],[3,4,1]]}""",
      { Frame.Empty with
            faces =
                { Faces.Empty with
                      vertices = Some [ [ 1; 2; 3 ]; [ 3; 4; 1 ] ] } }
      """{"frame_unit":"unit","faces_edges":[[1,2,3],[3,4,1]]}""",
      { Frame.Empty with
            faces =
                { Faces.Empty with
                      edges = Some [ [ 1; 2; 3 ]; [ 3; 4; 1 ] ] } }
      """{"frame_unit":"unit","faceOrders":[[0,2,-1],[1,4,0]]}""",
      { Frame.Empty with
            faces =
                { Faces.Empty with
                      orders = Some [ (0, 2, -1); (1, 4, 0) ] } } ]


let deserializationTestCases = Util.toTest testCases

[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = FrameJson.FromJson source


let serializationTestCases = Util.toTestReverse testCases

[<TestCaseSource("serializationTestCases")>]
let Serialization source = FrameJson.ToJsonUnformatted source
