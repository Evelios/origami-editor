module FoldTests.FrameTests

open Geometry
open NUnit.Framework
open FsCheck
open FsCheck.NUnit

open Fold

[<SetUp>]
let Setup () = Gen.ArbFold.Register()

let testCases =
    [ """{"frame_unit":"unit"}""",
      { Frame.empty with
            Unit = LengthUnit.Unitless }
      """{"frame_author":"The Author","frame_unit":"unit"}""",
      { Frame.empty with
            Author = "The Author" }

      """{"frame_title":"The Title","frame_unit":"unit"}""", { Frame.empty with Title = "The Title" }

      """{"frame_description":"The Description","frame_unit":"unit"}""",
      { Frame.empty with
            Description = "The Description" }

      """{"frame_classes":["creasePattern","graph"],"frame_unit":"unit"}""",
      { Frame.empty with
            Classes =
                Set.ofList [ FrameClass.CreasePattern
                             FrameClass.Graph ] }

      """{"frame_attributes":["2D","orientable"],"frame_unit":"unit"}""",
      { Frame.empty with
            Attributes =
                Set.ofList [ FrameAttribute.Geo2D
                             FrameAttribute.Orientable ] }
      """{"frame_unit":"unit","vertices_coords":[[-1,-1],[1,1]]}""",
      { Frame.empty with
            Vertices =
                { Vertices.empty with
                      Coordinates = [ Point2D.xy -1. -1.; Point2D.xy 1. 1. ] } }
      """{"frame_unit":"unit","vertices_coords":[[0.5,1],[2.5,2.5]]}""",
      { Frame.empty with
            Vertices =
                { Vertices.empty with
                      Coordinates =
                          [ Point2D.xy 0.5 1.
                            Point2D.xy 2.5 2.5 ] } }
      """{"frame_unit":"unit","vertices_vertices":[0,1,2]}""",
      { Frame.empty with
            Vertices =
                { Vertices.empty with
                      Vertices = [ 0; 1; 2 ] } }
      """{"frame_unit":"unit","vertices_faces":[[0,1,2]]}""",
      { Frame.empty with
            Vertices =
                { Vertices.empty with
                      Faces = [ [ 0; 1; 2 ] ] } }
      """{"frame_unit":"unit","edges_vertices":[[1,0],[0,2]]}""",
      { Frame.empty with
            Edges =
                { Edges.empty with
                      Vertices = [ (1, 0); (0, 2) ] } }
      """{"frame_unit":"unit","edges_faces":[[1,2],[2,3]]}""",
      { Frame.empty with
            Edges =
                { Edges.empty with
                      Faces = [ (1, Some 2); (2, Some 3) ] } }
      """{"frame_unit":"unit","edges_faces":[[1,null],[2,null]]}""",
      { Frame.empty with
            Edges =
                { Edges.empty with
                      Faces = [ (1, None); (2, None) ] } }
      """{"frame_unit":"unit","edges_assignment":["B","U"]}""",
      { Frame.empty with
            Edges =
                { Edges.empty with
                      Assignment =
                          [ EdgeAssignment.Boundary
                            EdgeAssignment.Unassigned ] } }
      """{"frame_unit":"unit","edges_foldAngle":[0.7,0.1]}""",
      { Frame.empty with
            Edges =
                { Edges.empty with
                      FoldAngle = [ 0.7; 0.1 ] } }
      """{"frame_unit":"unit","edges_length":[10,20]}""",
      { Frame.empty with
            Edges =
                { Edges.empty with
                      Length = [ 10.; 20. ] } }
      """{"frame_unit":"unit","faces_vertices":[[1,2,3],[3,4,1]]}""",
      { Frame.empty with
            Faces =
                { Faces.empty with
                      Vertices = [ [ 1; 2; 3 ]; [ 3; 4; 1 ] ] } }
      """{"frame_unit":"unit","faces_edges":[[1,2,3],[3,4,1]]}""",
      { Frame.empty with
            Faces =
                { Faces.empty with
                      Edges = [ [ 1; 2; 3 ]; [ 3; 4; 1 ] ] } }
      """{"frame_unit":"unit","faceOrders":[[0,2,-1],[1,4,0]]}""",
      { Frame.empty with
            Faces =
                { Faces.empty with
                      Orders = [ (0, 2, -1); (1, 4, 0) ] } } ]


let deserializationTestCases = Util.toTest testCases

[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = Frame.fromJson source


let serializationTestCases = Util.toTestReverse testCases

[<TestCaseSource("serializationTestCases")>]
let Serialization source = Frame.toJsonUnformatted source


[<Property>]
let ``Serialize and Deserialize`` frame =
    frame
    |> Frame.toJson
    |> Frame.fromJson
    |> (=) frame
