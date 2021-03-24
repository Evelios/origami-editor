module FoldTests.EdgesTest


open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()

let testCases =
    [ """{"edges_vertices":[[1,0],[0,2]]}""",
      { Edges.Empty with
            vertices = Some [ (1, 0); (0, 2) ] }
      """{"edges_faces":[[1,2],[2,3]]}""",
      { Edges.Empty with
            faces = Some [ (1, Some 2); (2, Some 3) ] }
      """{"edges_faces":[[1,null],[2,null]]}""",
      { Edges.Empty with
            faces = Some [ (1, None); (2, None) ] }
      """{"edges_assignment":["B","U"]}""",
      { Edges.Empty with
            assignment =
                Some [ EdgeAssignment.Boundary
                       EdgeAssignment.Unassigned ] }
      """{"edges_foldAngle":[0.7,0.1]}""",
      { Edges.Empty with
            foldAngle = Some [ 0.7; 0.1 ] }
      """{"edges_length":[10,20]}""",
      { Edges.Empty with
            length = Some [ 10.; 20. ] } ]

let deserializationTestCases = Util.toTest testCases

[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = Edges.FromJson source


let serializationTestCases = Util.toTestReverse testCases

[<TestCaseSource("serializationTestCases")>]
let Serialization source = Edges.ToJsonUnformatted source
