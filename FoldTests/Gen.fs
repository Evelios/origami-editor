module FoldTests.Gen

open FsCheck

open Fold
open Utilities

let frameClasses = Gen.setOfType<FrameClass>

let frameAttributes = Gen.setOfType<FrameAttribute>

let lengthUnit = Gen.ofType<LengthUnit>

let frame =
    Gen.map6
        (fun author title description classes attributes unit ->
            Frame.empty
            |> Frame.setAuthor author
            |> Frame.setTitle title
            |> Frame.setDescription description
            |> Frame.setClasses classes
            |> Frame.setAttributes attributes
            |> Frame.setUnit unit)
        Gen.string
        Gen.string
        Gen.string
        frameClasses
        frameAttributes
        lengthUnit

let foldClasses = Gen.setOfType<FileClass>

let fold =
    Gen.map7
        (fun creator author title description classes keyFrame frames ->
            Fold.empty
            |> Fold.setCreator creator
            |> Fold.setAuthor author
            |> Fold.setTitle title
            |> Fold.setDescription description
            |> Fold.setClasses classes
            |> Fold.setKeyFrame keyFrame
            |> Fold.setFrames frames)
        Gen.string
        Gen.string
        Gen.string
        Gen.string
        foldClasses
        frame
        (Gen.listOf frame)

type ArbFold =
    static member Frame() = Arb.fromGen frame
    static member Fold() = Arb.fromGen fold
    static member Register() = Arb.register<ArbFold> () |> ignore
