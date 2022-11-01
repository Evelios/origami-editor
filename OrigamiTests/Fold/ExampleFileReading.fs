module OrigamiTests.ExampleFileReading

open System.IO
open Math.Units.Test
open NUnit.Framework

open Origami
open Origami.Fold

[<SetUp>]
let Setup () = ()

let exampleFoldFiles =
    Directory.GetFiles(Path.Combine(__SOURCE_DIRECTORY__, "Examples"), "*.fold")
    |> Array.map Util.toTestCaseData

[<TestCaseSource("exampleFoldFiles")>]
let exampleFileParsing path =
    Assert.DoesNotThrow (fun () ->
        File.ReadAllText path
        |> Fold.fromJson<TestSpace>
        |> ignore)
