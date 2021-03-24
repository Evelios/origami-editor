module FoldFileTests.FileTests

open FoldTests
open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()

let testCases =
    [ """{"file_spec":1}""", { FoldFile.Empty with spec = Some 1 }

      """{"file_creator":"The Creator"}""",
      { FoldFile.Empty with
            creator = Some "The Creator" }

      """{"file_author":"The Author"}""",
      { FoldFile.Empty with
            author = Some "The Author" }

      """{"file_title":"The Title"}""",
      { FoldFile.Empty with
            title = Some "The Title" }

      """{"file_description":"The Description"}""",
      { FoldFile.Empty with
            description = Some "The Description" }

      """{"file_classes":["singleModel"]}""",
      { FoldFile.Empty with
            classes = Some [ FileClass.SingleModel ] } ]


let deserializationTestCases = Util.toTest testCases

[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = FoldFile.FromJson source

let serializationTestCases = Util.toTestReverse testCases

[<TestCaseSource("serializationTestCases")>]
let Serialization source = FoldFile.ToJsonUnformatted source
