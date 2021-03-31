namespace Fold

open FSharp.Json

type FoldFileJson =
    { fileSpec: int option
      fileCreator: string option
      fileAuthor: string option
      fileTitle: string option
      fileDescription: string option
      fileClasses: FileClass list option
      fileFrames: FrameJson list option
      frameAuthor: string option
      frameTitle: string option
      frameDescription: string option
      frameClasses: FrameClass list option
      frameAttributes: FrameAttribute list option
      frameUnit: Unit option
      [<JsonField(Transform = typeof<Vertex.ListTransform>)>]
      verticesCoords: (Vertex list) option
      verticesVertices: int list option
      verticesFaces: int list list option
      edgesVertices: ((int * int) list) option
      edgesFaces: ((int * int option) list) option
      edgesAssignment: EdgeAssignment list option
      edgesFoldAngle: float list option
      edgesLength: float list option
      edgeOrders: ((int (*edge id*)  * int (*edge id*)  * int (*order*) ) list) option
      facesVertices: int list list option
      facesEdges: int list list option
      faceOrders: ((int (*face id*)  * int (*face id*)  * int (*order*) ) list) option }

module FoldJson =
    // Convert the fold file to a json serializable type
    let toJsonType (fold: Fold): FoldFileJson =
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

        { fileSpec = Some fold.spec
          fileCreator = fold.creator |> orNone
          fileAuthor = fold.author |> orNone
          fileTitle = fold.title |> orNone
          fileDescription = fold.description |> orNone
          fileClasses = fold.classes |> setWithDefault
          fileFrames = listWithDefault (List.map FrameJson.toJsonType fold.frames)
          frameAuthor = fold.keyFrame.author |> orNone
          frameTitle = fold.keyFrame.title |> orNone
          frameDescription = fold.keyFrame.description |> orNone
          frameClasses = fold.keyFrame.classes |> setWithDefault
          frameAttributes = fold.keyFrame.attributes |> setWithDefault
          frameUnit = Some fold.keyFrame.unit
          verticesCoords = fold.keyFrame.vertices.coords |> listWithDefault
          verticesVertices = fold.keyFrame.vertices.vertices |> listWithDefault
          verticesFaces = fold.keyFrame.vertices.faces |> listWithDefault
          edgesVertices = fold.keyFrame.edges.vertices |> listWithDefault
          edgesFaces = fold.keyFrame.edges.faces |> listWithDefault
          edgesAssignment = fold.keyFrame.edges.assignment |> listWithDefault
          edgesFoldAngle = fold.keyFrame.edges.foldAngle |> listWithDefault
          edgesLength = fold.keyFrame.edges.length |> listWithDefault
          edgeOrders = fold.keyFrame.edges.orders |> listWithDefault
          facesVertices = fold.keyFrame.faces.vertices |> listWithDefault
          facesEdges = fold.keyFrame.faces.edges |> listWithDefault
          faceOrders = fold.keyFrame.faces.orders |> listWithDefault }

    /// Convert the json serializable type to the foldFile type
    let fromJsonType (foldJson: FoldFileJson): Fold =
        let orEmptyString = Option.defaultValue ""

        let toSet =
            function
            | None -> Set.empty
            | Some list -> Set.ofList list

        { spec = foldJson.fileSpec |> Option.defaultValue 1
          creator = foldJson.fileCreator |> orEmptyString
          author = foldJson.fileAuthor |> orEmptyString
          title = foldJson.fileTitle |> orEmptyString
          description = foldJson.fileDescription |> orEmptyString
          classes = foldJson.fileClasses |> toSet
          keyFrame =
              FrameJson.fromJsonType
                  { frameAuthor = foldJson.frameAuthor
                    frameTitle = foldJson.frameTitle
                    frameDescription = foldJson.frameDescription
                    frameClasses = foldJson.frameClasses
                    frameAttributes = foldJson.frameAttributes
                    frameUnit = foldJson.frameUnit
                    verticesCoords = foldJson.verticesCoords
                    verticesVertices = foldJson.verticesVertices
                    verticesFaces = foldJson.verticesFaces
                    edgesVertices = foldJson.edgesVertices
                    edgesFaces = foldJson.edgesFaces
                    edgesAssignment = foldJson.edgesAssignment
                    edgesFoldAngle = foldJson.edgesFoldAngle
                    edgesLength = foldJson.edgesLength
                    edgeOrders = foldJson.edgeOrders
                    facesVertices = foldJson.facesVertices
                    facesEdges = foldJson.facesEdges
                    faceOrders = foldJson.faceOrders }
          frames =
              foldJson.fileFrames
              |> Option.defaultValue []
              |> List.map FrameJson.fromJsonType }

    let private jsonConfig =
        JsonConfig.create (jsonFieldNaming = FrameJson.nameConversion, serializeNone = SerializeNone.Omit)

    let private jsonConfigUnformatted =
        JsonConfig.create
            (jsonFieldNaming = FrameJson.nameConversion, serializeNone = SerializeNone.Omit, unformatted = true)

    let ToJson (fold: Fold): string =
        Json.serializeEx jsonConfig (toJsonType fold)

    let ToJsonUnformatted (fold: Fold): string =
        Json.serializeEx jsonConfigUnformatted (toJsonType fold)

    let FromJson json: Fold =
        Json.deserializeEx<FoldFileJson> jsonConfig json
        |> fromJsonType
