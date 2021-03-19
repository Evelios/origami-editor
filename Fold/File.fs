namespace Fold

open FSharp.Json


module FileMetadata =
    type FileClass =
        | SingleModel = 0
        | MultiModel = 1
        | Animation = 2
        | Diagrams = 3

    let private fileClassConverter =
        StringMap [ FileClass.SingleModel, "singleModel"
                    FileClass.MultiModel, "multiModel"
                    FileClass.Animation, "animation"
                    FileClass.Diagrams, "diagrams" ]

    type FileClassTransform =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<string>) ()

            member this.fromTargetType text =
                text.ToString() |> fileClassConverter.FromString :> obj

            member this.toTargetType fileClass =
                fileClassConverter.ToString(fileClass :?> FileClass) :> obj

type File =
    { [<JsonField(Transform = typeof<Version.Transform>)>]
      spec: Version option
      creator: string option
      author: string option
      title: string option
      description: string option
      [<JsonField(Transform = typeof<FileMetadata.FileClassTransform>)>]
      classes: FileMetadata.FileClass list option }

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
        JsonConfig.create (jsonFieldNaming = (+) "file_", serializeNone = SerializeNone.Omit)

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
