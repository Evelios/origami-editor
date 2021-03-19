module FoldTests.File

open NUnit.Framework
open Fold

[<SetUp>]
let Setup () = ()

let deserializationTestCases =
    [ """{ "file_spec": "1" }""",
      { File.Empty with
            spec = Some(Version.CreateWithMajor 1) }
      """{ "file_spec": "1.1" }""",
      { File.Empty with
            spec = Some(Version.CreateWithMinor 1 1) }
      """{ "file_spec": "1.1.1" }""",
      { File.Empty with
            spec = Some(Version.Create 1 1 1) }
      """{ "file_creator": "The Creator" }""",
      { File.Empty with
            creator = Some "The Creator" }
      """{ "file_author": "The Author" }""",
      { File.Empty with
            author = Some "The Author" }
      """{ "file_title": "The Title" }""",
      { File.Empty with
            title = Some "The Title" }
      """{ "file_description": "The Description" }""",
      { File.Empty with
            description = Some "The Description" }
      """{ "file_classes": ["singleModel"] }""",
      { File.Empty with
            classes = Some [ FileClass.singleModel ] } ]
    |> List.map (fun (d, r) -> TestCaseData(d).Returns r)

[<TestCaseSource("deserializationTestCases")>]
let Deserialization source = File.FromJson source

let serializationTestCases =
    [ { File.Empty with
            spec = Some(Version.CreateWithMajor 1) },
      """{"file_spec":"1"}"""
      { File.Empty with
            spec = Some(Version.CreateWithMinor 1 1) },
      """{"file_spec":"1.1"}"""
      { File.Empty with
            spec = Some(Version.Create 1 1 1) },
      """{"file_spec":"1.1.1"}"""
      { File.Empty with
            creator = Some "The Creator" },
      """{"file_creator":"The Creator"}"""
      { File.Empty with
            author = Some "The Author" },
      """{"file_author":"The Author"}"""
      { File.Empty with
            title = Some "The Title" },
      """{"file_title":"The Title"}"""
      { File.Empty with
            description = Some "The Description" },
      """{"file_description":"The Description"}"""
      { File.Empty with
            classes = Some [ FileClass.singleModel ] },
      """{"file_classes":["singleModel"]}""" ]
    |> List.map (fun (d, r) -> TestCaseData(d).Returns r)

[<TestCaseSource("serializationTestCases")>]
let Serialization source = File.ToJsonUnformatted source
