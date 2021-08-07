module FoldTests.ExampleFileReading

open System.IO
open NUnit.Framework
open Fold
open Fold.Json

[<SetUp>]
let Setup () = ()

let exampleFoldFiles =
    Directory.GetFiles(Path.Combine(__SOURCE_DIRECTORY__, "Examples"), "*.fold")
    |> Array.map Util.toTestCaseData

[<TestCaseSource("exampleFoldFiles")>]
let exampleFileParsing path =
    Assert.DoesNotThrow(fun () ->
        File.ReadAllText path
        |> FoldJson.fromJson
        |> ignore)
