module FoldTests.FrameTests

open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()

let testCases =
    [ """{"frame_author":"The Author"}""",
      { Frame.Empty with
            author = Some "The Author" }

      """{"frame_title":"The Title"}""",
      { Frame.Empty with
            title = Some "The Title" }

      """{"frame_description":"The Description"}""",
      { Frame.Empty with
            description = Some "The Description" }

      """{"frame_classes":["creasePattern","graph"]}""",
      { Frame.Empty with
            classes =
                Some [ FrameMetadata.FrameClass.CreasePattern
                       FrameMetadata.FrameClass.Graph ] }

      """{"frame_attributes":["2D","orientable"]}""",
      { Frame.Empty with
            attributes =
                Some [ FrameMetadata.FrameAttribute.Geo2D
                       FrameMetadata.FrameAttribute.Orientable ] }

      """{"frame_unit":"unit"}""",
      { Frame.Empty with
            unit = Some FrameMetadata.Unit.Unitless } ]

let deserializationTestCases = Util.toTest testCases

[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = Frame.FromJson source


let serializationTestCases = Util.toTestReverse testCases

[<TestCaseSource("serializationTestCases")>]
let Serialization source = Frame.ToJsonUnformatted source
