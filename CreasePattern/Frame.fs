namespace CreasePattern

open Geometry

type Frame =
    { unit: Fold.LengthUnit
      author: string
      title: string
      description: string
      creasePattern: CreasePattern }

module Frame =

    (* Builders *)

    let create : Frame =
        { unit = Fold.Unitless
          author = ""
          title = ""
          description = ""
          creasePattern = CreasePattern.create }

    let empty : Frame =
        { unit = Fold.Unitless
          author = ""
          title = ""
          description = ""
          creasePattern = CreasePattern.empty }

    (* Modifiers *)

    let setUnit unit frame = { frame with unit = unit }

    let setAuthor author frame = { frame with author = author }
    let setTitle title frame = { frame with title = title }

    let setDescription description frame =
        { frame with description = description }

    let mapCreasePattern action frame =
        { frame with
              creasePattern = action frame.creasePattern }

    (* Serialization & Deserialization *)

    let fromFoldFrame (foldFrame: Fold.Frame) : Frame =
        { create with
              unit = foldFrame.unit
              creasePattern = CreasePattern.fromFoldValues foldFrame.vertices foldFrame.edges foldFrame.faces }

    let toFoldFrame (frame: Frame) : Fold.Frame =
        Fold.Frame.empty
        |> Fold.Frame.setUnit frame.unit
        |> Fold.Frame.setAuthor frame.author
        |> Fold.Frame.setTitle frame.title
        |> Fold.Frame.setDescription frame.description
        |> CreasePattern.addToFoldFrame frame.creasePattern
