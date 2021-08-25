module OrigamiBenchmarks.ReferenceFinderBenchmarks

open BenchmarkDotNet.Attributes

open CreasePattern
open Geometry

[<MemoryDiagnoser>]
type ReferenceFinderComparison() =

    [<Benchmark>]
    member this.Search() =
        ReferenceFinder.init
        |> ReferenceFinder.bestFoldSequenceTo (Point2D.xy 0.5 0.5)
        |> ignore
