module FoldFileTests.FileTests

open FoldTests
open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()

let testCases =
    [ """{"file_spec":1,"file_keyFrame":{"file_unit":"unit"}}""", { FoldFile.Empty with spec = 1 }

      """{"file_spec":1,"file_creator":"The Creator","file_keyFrame":{"file_unit":"unit"}}""",
      { FoldFile.Empty with
            creator = "The Creator" }

      """{"file_spec":1,"file_author":"The Author","file_keyFrame":{"file_unit":"unit"}}""",
      { FoldFile.Empty with
            author = "The Author" }

      """{"file_spec":1,"file_title":"The Title","file_keyFrame":{"file_unit":"unit"}}""",
      { FoldFile.Empty with
            title = "The Title" }

      """{"file_spec":1,"file_description":"The Description","file_keyFrame":{"file_unit":"unit"}}""",
      { FoldFile.Empty with
            description = "The Description" }

      """{"file_spec":1,"file_classes":["singleModel"],"file_keyFrame":{"file_unit":"unit"}}""",
      { FoldFile.Empty with
            classes = Set.ofList [ FileClass.SingleModel ] } ]


let deserializationTestCases = Util.toTest testCases

[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = FoldFile.FromJson source

let serializationTestCases = Util.toTestReverse testCases

[<TestCaseSource("serializationTestCases")>]
let Serialization source = FoldFile.ToJsonUnformatted source
