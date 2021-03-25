namespace Utilities

open Microsoft.FSharp.Reflection

module DiscriminatedUnion =
    let allCases<'T>() =
        FSharpType.GetUnionCases(typeof<'T>)
            |> Seq.map (fun x -> FSharpValue.MakeUnion(x, Array.zeroCreate(x.GetFields().Length)) :?> 'T)
