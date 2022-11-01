[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.Fold.Fold

open Thoth.Json.Net

open Origami.Fold
open Utilities.Extensions


// ---- Builders ------------------------------------------------------------------------

let create a : Fold<'Coordiantes> = a

let empty<'Coordinates> : Fold<'Coordinates> =
    create
        { Spec = 1
          Creator = ""
          Author = ""
          Title = ""
          Description = ""
          Classes = Set.empty
          KeyFrame = Frame.empty
          Frames = [] }


// ---- Constants --------------------------------------------------------------

let extensions =
    seq {
        "fold"
        "json"
    }


// ---- Modifiers --------------------------------------------------------------

let setSpec spec file : Fold<'Coordinates> = { file with Spec = spec }

let setCreator creator file : Fold<'Coordinates> = { file with Creator = creator }
let setAuthor author file : Fold<'Coordinates> = { file with Author = author }
let setTitle title file : Fold<'Coordinates> = { file with Title = title }
let setDescription description file : Fold<'Coordinates> = { file with Description = description }
let setClasses classes file : Fold<'Coordinates> = { file with Classes = classes }

let addClass theClass file : Fold<'Coordinates> =
    { file with Classes = Set.add theClass file.Classes }

let removeClass theClass file : Fold<'Coordinates> =
    { file with Classes = Set.remove theClass file.Classes }

let withoutClasses file : Fold<'Coordinates> = { file with Classes = Set.empty }
let setKeyFrame keyFrame file : Fold<'Coordinates> = { file with KeyFrame = keyFrame }
let setFrames frames file : Fold<'Coordinates> = { file with Frames = frames }


/// Try to update a particular frame. If an index is not contained within the frame, then nothing happens
/// This functions existence probably indicates that a dictionary structure is a better representation
/// of the fold fold frames.
let updateFrame
    (frameIndex: int)
    (update: Frame<'Coordinates> -> Frame<'Coordinates>)
    (fold: Fold<'Coordinates>)
    : Fold<'Coordinates> =
    if frameIndex = 0 then
        { fold with KeyFrame = update fold.KeyFrame }

    elif frameIndex - 1 >= fold.Frames.Length then
        fold

    else
        let frames =
            (List.mapi (fun i frame ->
                if i = (frameIndex - 1) then
                    update frame
                else
                    frame))
                fold.Frames

        setFrames frames fold


// ---- Serialization & Deserialization ----------------------------------------

let decoder<'Coordinates> : Decoder<Fold<'Coordinates>> =
    Decode.object (fun get ->
        let maybeStr name =
            get.Optional.Field name Decode.string
            |> Option.defaultValue ""

        let frames =
            get.Optional.Field "file_frames" (Decode.list Frame.decoder)
            |> Option.defaultValue []


        let baseFrame = Frame.decode get

        // Figure out which frame is the key frame
        let keyFrame, otherFrames =
            if baseFrame <> Frame.empty then
                baseFrame, frames

            else
                match frames with
                | [] -> Frame.empty, []
                | keyFrame :: otherFrames -> keyFrame, otherFrames


        let classes =
            get.Optional.Field "file_classes" (Decode.list Decode.string)
            |> Option.defaultValue []
            |> List.map FileClass.fromString
            |> List.filterNone
            |> Set.ofList


        { Spec = 1
          Creator = maybeStr "file_creator"
          Author = maybeStr "file_author"
          Title = maybeStr "file_title"
          Description = maybeStr "file_description"
          Classes = classes
          KeyFrame = keyFrame
          Frames = otherFrames })

let encode (fold: Fold<'Coordinates>) : JsonValue =
    let fileClasses =
        fold.Classes
        |> Seq.map (FileClass.toString >> Encode.string)
        |> Encode.seq

    let frames =
        fold.KeyFrame :: fold.Frames
        |> Seq.map Frame.encode
        |> Encode.seq

    Encode.object [
        if fold.Creator <> "" then
            "file_creator", Encode.string fold.Creator

        if fold.Author <> "" then
            "file_author", Encode.string fold.Author

        if fold.Title <> "" then
            "file_title", Encode.string fold.Title

        if fold.Description <> "" then
            "file_description", Encode.string fold.Description

        if fold.Classes <> Set.empty then
            "file_classes", fileClasses

        "file_frames", frames
    ]

let toJson (fold: Fold<'Coordinates>) : string = Encode.toString 0 (encode fold)

let fromJson<'Coordinates> (json: string) : Result<Fold<'Coordinates>, string> = Decode.fromString decoder json
