namespace OrigamiBenchmarks

open OrigamiBenchmarks.ReferenceFinderBenchmarks

module Program =
    open BenchmarkDotNet.Running
    open BenchmarkDotNet.Configs

    [<EntryPoint>]
    let main _ =
        BenchmarkRunner.Run<ReferenceFinderComparison> DefaultConfig.Instance
        |> ignore

        0
