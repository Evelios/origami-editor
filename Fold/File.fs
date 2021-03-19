namespace Fold

open FSharp.Json



type FileClass =
    | singleModel = 0
    | multiModel = 1
    | animation = 2
    | diagrams = 3
    | custom = 4

type File =
    { [<JsonField(Transform = typeof<Version.Transform>)>]
      spec: Version option
      creator: string option
      author: string option
      title: string option
      description: string option
      classes: FileClass list option }

module File =

    let Create a: File = a

    let Empty =
        Create
            { spec = None
              creator = None
              author = None
              title = None
              description = None
              classes = None }


    (* Json *)

    let private jsonConfig =
        JsonConfig.create (jsonFieldNaming = (+) "file_", serializeNone = SerializeNone.Omit, enumValue = EnumMode.Name)

    let private jsonConfigUnformatted =
        JsonConfig.create
            (jsonFieldNaming = (+) "file_",
             serializeNone = SerializeNone.Omit,
             enumValue = EnumMode.Name,
             unformatted = true)

    let ToJson (fold: File): string = Json.serializeEx jsonConfig fold

    let ToJsonUnformatted (fold: File): string =
        Json.serializeEx jsonConfigUnformatted fold

    let FromJson = Json.deserializeEx<File> jsonConfig
