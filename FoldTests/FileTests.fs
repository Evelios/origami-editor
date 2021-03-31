module FoldFileTests.FileTests

open FoldTests
open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()

let testCases =
    [ """{"file_spec":1,"frame_unit":"unit"}""", { Fold.Empty with spec = 1 }

      """{"file_spec":1,"file_creator":"The Creator","frame_unit":"unit"}""",
      { Fold.Empty with
            creator = "The Creator" }

      """{"file_spec":1,"file_author":"The Author","frame_unit":"unit"}""",
      { Fold.Empty with
            author = "The Author" }

      """{"file_spec":1,"file_title":"The Title","frame_unit":"unit"}""",
      { Fold.Empty with
            title = "The Title" }

      """{"file_spec":1,"file_description":"The Description","frame_unit":"unit"}""",
      { Fold.Empty with
            description = "The Description" }

      """{"file_spec":1,"file_classes":["singleModel"],"frame_unit":"unit"}""",
      { Fold.Empty with
            classes = Set.ofList [ FileClass.SingleModel ] }
      """{"file_spec":1,"frame_author":"The Author","frame_unit":"unit"}""",
      { Fold.Empty with
            keyFrame =
                { Frame.Empty with
                      author = "The Author" } }
      """{"file_spec":1,"file_frames":[{"frame_author":"The Author","frame_unit":"unit"}],"frame_unit":"unit"}""",
      { Fold.Empty with
            frames =
                [ { Frame.Empty with
                        author = "The Author" } ] } ]

let deserializationTestCases = Util.toTest testCases

[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = FoldJson.FromJson source

let serializationTestCases = Util.toTestReverse testCases

[<TestCaseSource("serializationTestCases")>]
let Serialization source = FoldJson.ToJsonUnformatted source
