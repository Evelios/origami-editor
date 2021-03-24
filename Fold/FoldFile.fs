namespace Fold

open FSharp.Json

type FileClass =
    | [<JsonUnionCase("singleModel")>] SingleModel
    | [<JsonUnionCase("multiModel")>] MultiModel
    | [<JsonUnionCase("animation")>] Animation
    | [<JsonUnionCase("diagrams")>] Diagrams

type FoldFile =
    { spec: int option
      creator: string option
      author: string option
      title: string option
      description: string option
      classes: FileClass list option
      keyFrame: Frame option
      frames: Frame list option }

module FoldFile =

    let Create a: FoldFile = a

    let Empty =
        Create
            { spec = None
              creator = None
              author = None
              title = None
              description = None
              classes = None
              keyFrame = None
              frames = None }


    (* Modifiers *)

    let setSpec spec file: FoldFile = { file with spec = spec }

    let setCreator creator file: FoldFile = { file with creator = creator }
    let setAuthor author file: FoldFile = { file with author = author }
    let setTitle title file: FoldFile = { file with title = title }
    let setDescription description file: FoldFile = { file with description = description }
    let setClasses classes file: FoldFile = { file with classes = classes }
    let setKeyframe keyFrame file: FoldFile = { file with keyFrame = keyFrame }
    let setFrames frames file: FoldFile = { file with frames = frames }


    (* Json *)

    let private jsonConfig =
        JsonConfig.create (jsonFieldNaming = (+) "file_", serializeNone = SerializeNone.Omit)

    let private jsonConfigUnformatted =
        JsonConfig.create
            (jsonFieldNaming = (+) "file_",
             serializeNone = SerializeNone.Omit,
             enumValue = EnumMode.Name,
             unformatted = true)

    let ToJson (fold: FoldFile): string = Json.serializeEx jsonConfig fold

    let ToJsonUnformatted (fold: FoldFile): string =
        Json.serializeEx jsonConfigUnformatted fold

    let FromJson json =
        let file =
            Json.deserializeEx<FoldFile> jsonConfig json

        let keyFrame = Frame.FromJson json

        { file with
              keyFrame = if keyFrame = Frame.Empty then None else Some keyFrame }
