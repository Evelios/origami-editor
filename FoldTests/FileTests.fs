module FoldTests.FileTests

open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()

let testCases =
    [ """{"file_spec":"1"}""",
      { File.Empty with
            spec = Some(Version.CreateWithMajor 1) }

      """{"file_spec":"1.1"}""",
      { File.Empty with
            spec = Some(Version.CreateWithMinor 1 1) }

      """{"file_spec":"1.1.1"}""",
      { File.Empty with
            spec = Some(Version.Create 1 1 1) }

      """{"file_creator":"The Creator"}""",
      { File.Empty with
            creator = Some "The Creator" }

      """{"file_author":"The Author"}""",
      { File.Empty with
            author = Some "The Author" }

      """{"file_title":"The Title"}""",
      { File.Empty with
            title = Some "The Title" }

      """{"file_description":"The Description"}""",
      { File.Empty with
            description = Some "The Description" }

      """{"file_classes":["singleModel"]}""",
      { File.Empty with
            classes = Some [ FileMetadata.FileClass.SingleModel ] } ]


let deserializationTestCases = Util.toTest testCases
[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = File.FromJson source

let serializationTestCases = Util.toTestReverse testCases
[<TestCaseSource("serializationTestCases")>]
let Serialization source = File.ToJsonUnformatted source
