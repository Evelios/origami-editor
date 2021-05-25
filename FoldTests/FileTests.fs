module FoldFileTests.FileTests

open FoldTests
open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()


[<TestCase>]
let UpdateKeyFrame () =
    let expected =
        { Fold.empty with
              keyFrame =
                  { Frame.empty with
                        author = "The Author" } }

    let actual =
        Fold.updateFrame 0 (Frame.setAuthor "The Author") Fold.empty

    Assert.AreEqual(expected, actual)

[<TestCase>]
let UpdateFirstFrame () =
    let expected =
        { Fold.empty with
              frames =
                  [ { Frame.empty with
                          author = "The Author" } ] }

    let actual =
        Fold.updateFrame
            1
            (Frame.setAuthor "The Author")
            { Fold.empty with
                  frames = [ Frame.empty ] }

    Assert.AreEqual(expected, actual)

(* Serialization and Deserialization *)

let testCases =
    [ """{"file_spec":1,"frame_unit":"unit"}""", { Fold.empty with spec = 1 }

      """{"file_spec":1,"file_creator":"The Creator","frame_unit":"unit"}""",
      { Fold.empty with
            creator = "The Creator" }

      """{"file_spec":1,"file_author":"The Author","frame_unit":"unit"}""",
      { Fold.empty with
            author = "The Author" }

      """{"file_spec":1,"file_title":"The Title","frame_unit":"unit"}""", { Fold.empty with title = "The Title" }

      """{"file_spec":1,"file_description":"The Description","frame_unit":"unit"}""",
      { Fold.empty with
            description = "The Description" }

      """{"file_spec":1,"file_classes":["singleModel"],"frame_unit":"unit"}""",
      { Fold.empty with
            classes = Set.ofList [ FileClass.SingleModel ] }
      """{"file_spec":1,"frame_author":"The Author","frame_unit":"unit"}""",
      { Fold.empty with
            keyFrame =
                { Frame.empty with
                      author = "The Author" } }
      """{"file_spec":1,"file_frames":[{"frame_author":"The Author","frame_unit":"unit"}],"frame_unit":"unit"}""",
      { Fold.empty with
            frames =
                [ { Frame.empty with
                        author = "The Author" } ] } ]

let deserializationTestCases = Util.toTest testCases

[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = FoldJson.fromJson source

let serializationTestCases = Util.toTestReverse testCases

[<TestCaseSource("serializationTestCases")>]
let Serialization source = FoldJson.toJsonUnformatted source
