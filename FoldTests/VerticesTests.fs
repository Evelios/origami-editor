module FoldTests.VerticesTests

open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()

let testCases =
    [ """{"vertices_coords":[[0.5,1],[2.2,2.5]]}""",
      { Vertices.Empty with
            coords = Some [ (0.5, 1.); (2.2, 2.5) ] }
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
