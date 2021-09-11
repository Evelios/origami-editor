namespace Fold

open FSharp.Json
open Geometry

type FileClass =
    | [<JsonUnionCase("singleModel")>] SingleModel
    | [<JsonUnionCase("multiModel")>] MultiModel
    | [<JsonUnionCase("animation")>] Animation
    | [<JsonUnionCase("diagrams")>] Diagrams

type FoldFileJson =
    { FileSpec: int option
      FileCreator: string option
      FileAuthor: string option
      FileTitle: string option
      FileDescription: string option
      FileClasses: FileClass list option
      FileFrames: FrameJson list option
      FrameAuthor: string option
      FrameTitle: string option
      FrameDescription: string option
      FrameClasses: FrameClass list option
      FrameAttributes: FrameAttribute list option
      FrameUnit: LengthUnit option
      [<JsonField(Transform = typeof<Point2D.ListTransform>)>]
      VerticesCoords: Point2D list option
      VerticesVertices: int list option
      VerticesFaces: int list list option
      EdgesVertices: (int * int) list option
      EdgesFaces: (int * int option) list option
      EdgesAssignment: EdgeAssignment list option
      EdgesFoldAngle: float list option
      EdgesLength: float list option
      EdgeOrders: (int (*edge id*)  * int (*edge id*)  * int (*order*) ) list option
      FacesVertices: int list list option
      FacesEdges: int list list option
      FaceOrders: (int (*face id*)  * int (*face id*)  * int (*order*) ) list option }

type Fold =
    { Spec: int
      Creator: string
      Author: string
      Title: string
      Description: string
      Classes: FileClass Set
      KeyFrame: Frame
      Frames: Frame list }

module Fold =

    let create a : Fold = a

    let empty =
        create
            { Spec = 1
              Creator = ""
              Author = ""
              Title = ""
              Description = ""
              Classes = Set.empty
              KeyFrame = Frame.empty
              Frames = [] }

    (* Constants *)

    let extensions =
        seq {
            "fold"
            "json"
        }


    (* Modifiers *)

    let setSpec spec file : Fold = { file with Spec = spec }

    let setCreator creator file : Fold = { file with Creator = creator }
    let setAuthor author file : Fold = { file with Author = author }
    let setTitle title file : Fold = { file with Title = title }
    let setDescription description file : Fold = { file with Description = description }
    let setClasses classes file : Fold = { file with Classes = classes }

    let addClass theClass file : Fold =
        { file with
              Classes = Set.add theClass file.Classes }

    let removeClass theClass file : Fold =
        { file with
              Classes = Set.remove theClass file.Classes }

    let withoutClasses file : Fold = { file with Classes = Set.empty }
    let setKeyFrame keyFrame file : Fold = { file with KeyFrame = keyFrame }
    let setFrames frames file : Fold = { file with Frames = frames }


    (**
    Try to update a particular frame. If an index is not contained within the frame, then nothing happens
    This functions existence probably indicates that a dictionary structure is a better representation
    of the fold fold frames.
    *)
    let updateFrame (frameIndex: int) (update: Frame -> Frame) (fold: Fold) : Fold =
        if frameIndex = 0 then
            { fold with
                  KeyFrame = update fold.KeyFrame }

        elif frameIndex - 1 >= fold.Frames.Length then
            fold

        else
            let frames =
                (List.mapi
                    (fun i frame ->
                        if i = (frameIndex - 1) then
                            update frame
                        else
                            frame))
                    fold.Frames

            setFrames frames fold


    (* Json Serialization & Deserialization *)

    // Convert the fold file to a json serializable type
    let toJsonType (fold: Fold) : FoldFileJson =
        let orNone =
            function
            | "" -> None
            | str -> Some str

        let (|EmptySet|_|) a = if Set.isEmpty a then Some() else None

        let setWithDefault =
            function
            | EmptySet -> None
            | set -> Some(Set.toList set)

        let listWithDefault =
            function
            | [] -> None
            | list -> Some list

        { FileSpec = Some fold.Spec
          FileCreator = fold.Creator |> orNone
          FileAuthor = fold.Author |> orNone
          FileTitle = fold.Title |> orNone
          FileDescription = fold.Description |> orNone
          FileClasses = fold.Classes |> setWithDefault
          FileFrames = listWithDefault (List.map Frame.toJsonType fold.Frames)
          FrameAuthor = fold.KeyFrame.Author |> orNone
          FrameTitle = fold.KeyFrame.Title |> orNone
          FrameDescription = fold.KeyFrame.Description |> orNone
          FrameClasses = fold.KeyFrame.Classes |> setWithDefault
          FrameAttributes = fold.KeyFrame.Attributes |> setWithDefault
          FrameUnit = Some fold.KeyFrame.Unit
          VerticesCoords =
              fold.KeyFrame.Vertices.Coordinates
              |> listWithDefault
          VerticesVertices = fold.KeyFrame.Vertices.Vertices |> listWithDefault
          VerticesFaces = fold.KeyFrame.Vertices.Faces |> listWithDefault
          EdgesVertices = fold.KeyFrame.Edges.Vertices |> listWithDefault
          EdgesFaces = fold.KeyFrame.Edges.Faces |> listWithDefault
          EdgesAssignment = fold.KeyFrame.Edges.Assignment |> listWithDefault
          EdgesFoldAngle = fold.KeyFrame.Edges.FoldAngle |> listWithDefault
          EdgesLength = fold.KeyFrame.Edges.Length |> listWithDefault
          EdgeOrders = fold.KeyFrame.Edges.Orders |> listWithDefault
          FacesVertices = fold.KeyFrame.Faces.Vertices |> listWithDefault
          FacesEdges = fold.KeyFrame.Faces.Edges |> listWithDefault
          FaceOrders = fold.KeyFrame.Faces.Orders |> listWithDefault }

    /// Convert the json serializable type to the foldFile type
    let fromJsonType (foldJson: FoldFileJson) : Fold =
        let orEmptyString = Option.defaultValue ""

        let toSet =
            function
            | None -> Set.empty
            | Some list -> Set.ofList list

        { Spec = foldJson.FileSpec |> Option.defaultValue 1
          Creator = foldJson.FileCreator |> orEmptyString
          Author = foldJson.FileAuthor |> orEmptyString
          Title = foldJson.FileTitle |> orEmptyString
          Description = foldJson.FileDescription |> orEmptyString
          Classes = foldJson.FileClasses |> toSet
          KeyFrame =
              Frame.fromJsonType
                  { FrameAuthor = foldJson.FrameAuthor
                    FrameTitle = foldJson.FrameTitle
                    FrameDescription = foldJson.FrameDescription
                    FrameClasses = foldJson.FrameClasses
                    FrameAttributes = foldJson.FrameAttributes
                    FrameUnit = foldJson.FrameUnit
                    VerticesCoords = foldJson.VerticesCoords
                    VerticesVertices = foldJson.VerticesVertices
                    VerticesFaces = foldJson.VerticesFaces
                    EdgesVertices = foldJson.EdgesVertices
                    EdgesFaces = foldJson.EdgesFaces
                    EdgesAssignment = foldJson.EdgesAssignment
                    EdgesFoldAngle = foldJson.EdgesFoldAngle
                    EdgesLength = foldJson.EdgesLength
                    EdgeOrders = foldJson.EdgeOrders
                    FacesVertices = foldJson.FacesVertices
                    FacesEdges = foldJson.FacesEdges
                    FaceOrders = foldJson.FaceOrders }
          Frames =
              foldJson.FileFrames
              |> Option.defaultValue []
              |> List.map Frame.fromJsonType }

    let private jsonConfig =
        JsonConfig.create (jsonFieldNaming = Frame.nameConversion, serializeNone = SerializeNone.Omit)

    let private jsonConfigUnformatted =
        JsonConfig.create (
            jsonFieldNaming = Frame.nameConversion,
            serializeNone = SerializeNone.Omit,
            unformatted = true
        )

    let toJson (fold: Fold) : string =
        Json.serializeEx jsonConfig (toJsonType fold)

    let toJsonUnformatted (fold: Fold) : string =
        Json.serializeEx jsonConfigUnformatted (toJsonType fold)

    let fromJson json : Fold =
        Json.deserializeEx<FoldFileJson> jsonConfig json
        |> fromJsonType
