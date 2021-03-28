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
                             FrameAttribute.Orientable ] } ]


let deserializationTestCases = Util.toTest testCases

[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = Frame.FromJson source


let serializationTestCases = Util.toTestReverse testCases

[<TestCaseSource("serializationTestCases")>]
let Serialization source = Frame.ToJsonUnformatted source
