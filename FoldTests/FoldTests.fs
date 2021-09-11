module FoldFileTests.FoldTests

open FoldTests
open NUnit.Framework
open FsCheck
open FsCheck.NUnit

open Fold

[<SetUp>]
let Setup () = Gen.ArbFold.Register()


[<TestCase>]
let UpdateKeyFrame () =
    let expected =
        { Fold.empty with
              KeyFrame =
                  { Frame.empty with
                        Author = "The Author" } }

    let actual =
        Fold.updateFrame 0 (Frame.setAuthor "The Author") Fold.empty

    Assert.AreEqual(expected, actual)

[<TestCase>]
let UpdateFirstFrame () =
    let expected =
        { Fold.empty with
              Frames =
                  [ { Frame.empty with
                          Author = "The Author" } ] }

    let actual =
        Fold.updateFrame
            1
            (Frame.setAuthor "The Author")
            { Fold.empty with
                  Frames = [ Frame.empty ] }

    Assert.AreEqual(expected, actual)

(* Serialization and Deserialization *)

let testCases =
    [ """{"file_spec":1,"frame_unit":"unit"}""", { Fold.empty with Spec = 1 }

      """{"file_spec":1,"file_creator":"The Creator","frame_unit":"unit"}""",
      { Fold.empty with
            Creator = "The Creator" }

      """{"file_spec":1,"file_author":"The Author","frame_unit":"unit"}""",
      { Fold.empty with
            Author = "The Author" }

      """{"file_spec":1,"file_title":"The Title","frame_unit":"unit"}""", { Fold.empty with Title = "The Title" }

      """{"file_spec":1,"file_description":"The Description","frame_unit":"unit"}""",
      { Fold.empty with
            Description = "The Description" }

      """{"file_spec":1,"file_classes":["singleModel"],"frame_unit":"unit"}""",
      { Fold.empty with
            Classes = Set.ofList [ FileClass.SingleModel ] }
      """{"file_spec":1,"frame_author":"The Author","frame_unit":"unit"}""",
      { Fold.empty with
            KeyFrame =
                { Frame.empty with
                      Author = "The Author" } }
      """{"file_spec":1,"file_frames":[{"frame_author":"The Author","frame_unit":"unit"}],"frame_unit":"unit"}""",
      { Fold.empty with
            Frames =
                [ { Frame.empty with
                        Author = "The Author" } ] } ]

let deserializationTestCases = Util.toTest testCases

[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = Fold.fromJson source

let serializationTestCases = Util.toTestReverse testCases

[<TestCaseSource("serializationTestCases")>]
let Serialization source = Fold.toJsonUnformatted source

[<Property>]
let ``Serialize and Deserialize`` fold =
    fold |> Fold.toJson |> Fold.fromJson |> (=) fold
