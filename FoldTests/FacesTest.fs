module FoldTests.FacesTest


open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()

let testCases =
    [ """{"faces_vertices":[[1,2,3],[3,4,1]]}""",
      { Faces.Empty with
            vertices = Some [ [ 1; 2; 3 ]; [ 3; 4; 1 ] ] }
      """{"faces_edges":[[1,2,3],[3,4,1]]}""",
      { Faces.Empty with
            edges = Some [ [ 1; 2; 3 ]; [ 3; 4; 1 ] ] }
      """{"faces_orders":[[0,2,-1],[1,4,0]]}""",
      { Faces.Empty with
            orders = Some [ (0, 2, -1); (1, 4, 0) ] } ]

let deserializationTestCases = Util.toTest testCases

[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = Faces.FromJson source


let serializationTestCases = Util.toTestReverse testCases

[<TestCaseSource("serializationTestCases")>]
let Serialization source = Faces.ToJsonUnformatted source
