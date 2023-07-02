module OrigamiBenchmarks.ReferenceFinderBenchmarks

open BenchmarkDotNet.Attributes

open Origami
open Math.Geometry

[<MemoryDiagnoser>]
type ReferenceFinderComparison() =

    [<Benchmark>]
    member this.BestFoldSequence() =
        ReferenceFinder.init
        |> ReferenceFinder.bestFoldSequenceTo (Point2D.meters 0.5 0.5)
        |> ignore

    [<Benchmark>]
    member this.Best1Sequences() =
        ReferenceFinder.init
        |> ReferenceFinder.bestFoldSequencesTo 1 (Point2D.meters 0.5 0.5)
        |> ignore

    [<Benchmark>]
    member this.Best5Sequences() =
        ReferenceFinder.init
        |> ReferenceFinder.bestFoldSequencesTo 5 (Point2D.meters 0.5 0.5)
        |> ignore
