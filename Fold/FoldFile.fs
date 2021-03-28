namespace Fold

open FSharp.Json

type FileClass =
    | [<JsonUnionCase("singleModel")>] SingleModel
    | [<JsonUnionCase("multiModel")>] MultiModel
    | [<JsonUnionCase("animation")>] Animation
    | [<JsonUnionCase("diagrams")>] Diagrams

type FoldFile =
    { spec: int
      creator: string
      author: string
      title: string
      description: string
      classes: FileClass Set
      keyFrame: Frame
      frames: Frame list }

type FoldFileJson =
    { spec: int option
      creator: string option
      author: string option
      title: string option
      description: string option
      classes: FileClass list option
      keyFrame: FrameJson option
      frames: FrameJson list option }

module FoldFile =

    let Create a: FoldFile = a

    let Empty =
        Create
            { spec = 1
              creator = ""
              author = ""
              title = ""
              description = ""
              classes = Set.empty
              keyFrame = Frame.Empty
              frames = [] }


    (* Modifiers *)

    let setSpec spec file: FoldFile = { file with spec = spec }

    let setCreator creator file: FoldFile = { file with creator = creator }
    let setAuthor author file: FoldFile = { file with author = author }
    let setTitle title file: FoldFile = { file with title = title }
    let setDescription description file: FoldFile = { file with description = description }
    let setClasses classes file: FoldFile = { file with classes = classes }
    let addClass theClass file: FoldFile = { file with classes = Set.add theClass file.classes } 
    let setKeyframe keyFrame file: FoldFile = { file with keyFrame = keyFrame }
    let setFrames frames file: FoldFile = { file with frames = frames }


    (**
    Try to update a particular frame. If an index is not contained within the frame, then nothing happens
    This functions existence probably indicates that a dictionary structure is a better representation
    of the fold fold frames.
    *)
    let updateFrame (frameIndex: int) (update: Frame -> Frame) (fold: FoldFile): FoldFile =
        let frames =
            (List.mapi (fun i frame -> if i = frameIndex then update frame else frame)) fold.frames

        setFrames frames fold


    (* Json *)

    // Convert the fold file to a json serializable type
    let toJsonType (fold: FoldFile): FoldFileJson =
        let stringWithDefault =
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

        { spec = Some fold.spec
          creator = stringWithDefault fold.creator
          author = stringWithDefault fold.author
          title = stringWithDefault fold.title
          description = stringWithDefault fold.description
          classes = setWithDefault fold.classes
          keyFrame = Some(Frame.toJsonType fold.keyFrame)
          frames = listWithDefault (List.map Frame.toJsonType fold.frames) }

    /// Convert the json serializable type to the foldFile type
    let fromJsonType (foldJson: FoldFileJson): FoldFile =
        let orEmptyString = Option.defaultValue ""

        let toSet =
            function
            | None -> Set.empty
            | Some list -> Set.ofList list

        { spec = foldJson.spec |> Option.defaultValue 1
          creator = foldJson.creator |> orEmptyString
          author = foldJson.author |> orEmptyString
          title = foldJson.title |> orEmptyString
          description = foldJson.description |> orEmptyString
          classes = foldJson.classes |> toSet
          keyFrame =
              foldJson.keyFrame
              |> Option.map Frame.fromJsonType
              |> Option.defaultValue Frame.Empty
          frames =
              foldJson.frames
              |> Option.defaultValue []
              |> List.map Frame.fromJsonType }



    let private jsonConfig =
        JsonConfig.create (jsonFieldNaming = (+) "file_", serializeNone = SerializeNone.Omit)

    let private jsonConfigUnformatted =
        JsonConfig.create
            (jsonFieldNaming = (+) "file_",
             serializeNone = SerializeNone.Omit,
             enumValue = EnumMode.Name,
             unformatted = true)

    let ToJson (fold: FoldFile): string =
        Json.serializeEx jsonConfig (toJsonType fold)

    let ToJsonUnformatted (fold: FoldFile): string =
        Json.serializeEx jsonConfigUnformatted (toJsonType fold)

    let FromJson json: FoldFile =
        let file =
            Json.deserializeEx<FoldFileJson> jsonConfig json

        { file with
              keyFrame = Frame.FromJson json |> Frame.toJsonType |> Some }
        |> fromJsonType
