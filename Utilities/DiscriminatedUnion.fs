namespace Utilities

open Microsoft.FSharp.Reflection

module DiscriminatedUnion =
    let allCases<'T> () =
        FSharpType.GetUnionCases(typeof<'T>)
        |> Seq.map (fun x -> FSharpValue.MakeUnion(x, Array.zeroCreate (x.GetFields().Length)) :?> 'T)

    let asIndexedMap<'T when 'T: comparison> (): Map<'T, int> =
        allCases<'T> ()
        |> Seq.mapi (fun i e -> e, i)
        |> Seq.toList
        |> Map.ofList

    let fromIndex<'T> (index:int): 'T =
        Seq.item index (allCases<'T>())
