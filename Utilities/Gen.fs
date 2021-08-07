namespace Utilities

open FsCheck
open Utilities.Extensions

module Gen =
    let map7 fn a b c d e f g =
        Gen.apply (Gen.apply (Gen.apply (Gen.apply (Gen.apply (Gen.apply (Gen.map fn a) b) c) d) e) f) g

    let floatBetween low high =
        let fraction (a, b, c) = double a + double b / double c

        Arb.generate<int>
        |> Gen.three
        |> Gen.filter (fun (_, _, c) -> c <> 0)
        |> Gen.map fraction


    let float =
        Arb.generate<NormalFloat> |> Gen.map float

    let string =
        Arb.generate<XmlEncodedString>
        |> Gen.map string
        |> Gen.filter (fun str -> str <> null)


    let ofType<'a> =
        Gen.elements DiscriminatedUnion.allCases<'a>

    let setOfType<'a when 'a: comparison> =
        Seq.toList DiscriminatedUnion.allCases<'a>
        |> Gen.elements
        |> Gen.listOf
        |> Gen.map Set.ofList
