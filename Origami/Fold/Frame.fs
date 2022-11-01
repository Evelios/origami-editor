[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.Fold.Frame

open Thoth.Json.Net

open Utilities.Extensions

// ---- Builders ------------------------------------------------------------------------

let create a : Frame<'Coordinates> = a

let empty: Frame<'Coordinates> =
    { Author = ""
      Title = ""
      Description = ""
      Classes = Set.empty
      Attributes = Set.empty
      Unit = LengthUnit.Unitless
      Vertices = Vertices.empty
      Edges = Edges.empty
      Faces = Faces.empty }


// ---- Modifiers --------------------------------------------------------------

let setAuthor author frame : Frame<'Coordinates> = { frame with Author = author }

let setTitle title frame : Frame<'Coordinates> = { frame with Title = title }

let setDescription description frame : Frame<'Coordinates> =
    { frame with Description = description }

let setClasses classes frame : Frame<'Coordinates> = { frame with Classes = classes }

let addClass theClass frame : Frame<'Coordinates> =
    { frame with Classes = Set.add theClass frame.Classes }

let removeClass theClass frame : Frame<'Coordinates> =
    { frame with Classes = Set.remove theClass frame.Classes }

let withoutClasses frame : Frame<'Coordinates> = { frame with Classes = Set.empty }
let setAttributes attributes frame : Frame<'Coordinates> = { frame with Attributes = attributes }

let addAttribute attribute frame : Frame<'Coordinates> =
    { frame with Attributes = Set.add attribute frame.Attributes }

let removeAttribute attribute frame : Frame<'Coordinates> =
    { frame with Attributes = Set.remove attribute frame.Attributes }

let withoutAttributes frame : Frame<'Coordinates> = { frame with Attributes = Set.empty }
let setUnit unit frame : Frame<'Coordinates> = { frame with Unit = unit }

let setVertices vertices frame : Frame<'Coordinates> = { frame with Vertices = vertices }
let setEdges edges frame : Frame<'Coordinates> = { frame with Edges = edges }
let setFaces faces frame : Frame<'Coordinates> = { frame with Faces = faces }


// ---- Serialization & Deserialization ----------------------------------------

let decode (get: Decode.IGetters) : Frame<'Coordinates> =
    let maybeStr name =
        get.Optional.Field name Decode.string
        |> Option.defaultValue ""

    let maybeSet name f =
        get.Optional.Field name (Decode.list Decode.string)
        |> Option.defaultValue []
        |> List.map f
        |> List.filterNone
        |> Set.ofList

    let unit name =
        get.Optional.Field name Decode.string
        |> Option.bind LengthUnit.fromString
        |> Option.defaultValue LengthUnit.Unitless

    { Author = maybeStr "frame_author"
      Title = maybeStr "frame_title"
      Description = maybeStr "frame_description"
      Classes = maybeSet "frame_classes" FrameClass.fromString
      Attributes = maybeSet "frame_attributes" FrameAttribute.fromString
      Unit = unit "frame_unit"
      Vertices = Vertices.decoder get
      Edges = Edges.decoder get
      Faces = Faces.decoder get }

let decoder<'Coordinates> : Decoder<Frame<'Coordinates>> =
    Decode.object decode

let encode (frame: Frame<'Coordinates>) : JsonValue =
    let encodeList f list =
        Seq.map (f >> Encode.string) list |> Encode.seq

    let frameValues =
        [ if frame.Author <> "" then
              "frame_author", Encode.string frame.Author

          if frame.Title <> "" then
              "frame_title", Encode.string frame.Title

          if frame.Description <> "" then
              "frame_description", Encode.string frame.Description

          if frame.Classes <> Set.empty then
              "frame_classes", encodeList FrameClass.toString frame.Classes

          if frame.Attributes <> Set.empty then
              "frame_attributes", encodeList FrameAttribute.toString frame.Attributes

          "frame_unit", Encode.string (LengthUnit.toString LengthUnit.Unitless) ]

    Encode.object (
        frameValues
        @ Vertices.encode frame.Vertices
          @ Edges.encode frame.Edges
            @ Faces.encode frame.Faces
    )

let toJson (frame: Frame<'Coordinates>) : string = Encode.toString 0 (encode frame)

let fromJson<'Coordinates> (json: string) : Result<Frame<'Coordinates>, string> = Decode.fromString decoder json
