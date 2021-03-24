module FoldTests.VerticesTests

open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()

let testCases =
    [ """{"vertices_coords":[[-1,-1],[1,1]]}""",
      { Vertices.Empty with
            coords =
                Some [ Vertex.in2d -1.f -1.f
                       Vertex.in2d 1.f 1.f ] }
      """{"vertices_coords":[[0.5,1],[2.5,2.5]]}""",
      { Vertices.Empty with
            coords =
                Some [ Vertex.in2d 0.5f 1.f
                       Vertex.in2d 2.5f 2.5f ] }
      """{"vertices_vertices":[0,1,2]}""",
      { Vertices.Empty with
            vertices = Some [ 0; 1; 2 ] }
      """{"vertices_faces":[[0,1,2]]}""",
      { Vertices.Empty with
            faces = Some [ [ 0; 1; 2 ] ] } ]

let deserializationTestCases = Util.toTest testCases

[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = Vertices.FromJson source


let serializationTestCases = Util.toTestReverse testCases

[<TestCaseSource("serializationTestCases")>]
let Serialization source = Vertices.ToJsonUnformatted source
