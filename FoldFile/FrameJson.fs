namespace Fold

open FSharp.Json

type FrameJson =
    { frameAuthor: string option
      frameTitle: string option
      frameDescription: string option
      frameClasses: FrameClass list option
      frameAttributes: FrameAttribute list option
      frameUnit: Unit option
      [<JsonField(Transform = typeof<Vertex.ListTransform>)>]
      verticesCoords: Vertex list option
      verticesVertices: int list option
      verticesFaces: int list list option
      edgesVertices: (int * int) list option
      edgesFaces: (int * int option) list option
      edgesAssignment: EdgeAssignment list option
      edgesFoldAngle: float list option
      edgesLength: float list option
      edgeOrders: (int (*edge id*)  * int (*edge id*)  * int (*order*) ) list option
      facesVertices: int list list option
      facesEdges: int list list option
      faceOrders: (int (*face id*)  * int (*face id*)  * int (*order*) ) list option }

module FrameJson =
    let nameConversion =
        function
        | "edgesFoldAngle" -> "edges_foldAngle"
        | "faceOrders" -> "faceOrders"
        | "edgeOrders" -> "edgeOrders"
        | name -> Json.snakeCase name


    /// Convert the frame type to a json serializable type
    let toJsonType (frame: Frame) : FrameJson =
        let stringWithDefault =
            function
            | "" -> None
            | str -> Some str

        let setWithDefault =
            let (|EmptySet|_|) a = if Set.isEmpty a then Some() else None

            function
            | EmptySet -> None
            | set -> Some(Set.toList set)

        let listWithDefault =
            function
            | [] -> None
            | list -> Some list

        { frameAuthor = stringWithDefault frame.author
          frameTitle = stringWithDefault frame.title
          frameDescription = stringWithDefault frame.description
          frameClasses = setWithDefault frame.classes
          frameAttributes = setWithDefault frame.attributes
          frameUnit = Some frame.unit
          verticesCoords = frame.vertices.coords |> listWithDefault
          verticesVertices = frame.vertices.vertices |> listWithDefault
          verticesFaces = frame.vertices.faces |> listWithDefault
          edgesVertices = frame.edges.vertices |> listWithDefault
          edgesFaces = frame.edges.faces |> listWithDefault
          edgesAssignment = frame.edges.assignment |> listWithDefault
          edgesFoldAngle = frame.edges.foldAngle |> listWithDefault
          edgesLength = frame.edges.length |> listWithDefault
          edgeOrders = frame.edges.orders |> listWithDefault
          facesVertices = frame.faces.vertices |> listWithDefault
          facesEdges = frame.faces.edges |> listWithDefault
          faceOrders = frame.faces.orders |> listWithDefault }

    /// Convert the json serializable type to the frame type
    let fromJsonType (frameJson: FrameJson) : Frame =
        let orEmptyString = Option.defaultValue ""

        let toSet =
            function
            | None -> Set.empty
            | Some list -> Set.ofList list

        { author = orEmptyString frameJson.frameAuthor
          title = orEmptyString frameJson.frameTitle
          description = orEmptyString frameJson.frameDescription
          classes = frameJson.frameClasses |> toSet
          attributes = frameJson.frameAttributes |> toSet
          unit =
              frameJson.frameUnit
              |> Option.defaultValue Unit.Unitless
          vertices =
              Vertices.Create
                  { coords = frameJson.verticesCoords |> Option.defaultValue []
                    vertices =
                        frameJson.verticesVertices
                        |> Option.defaultValue []
                    faces = frameJson.verticesFaces |> Option.defaultValue [] }
          edges =
              Edges.Create
                  { vertices = frameJson.edgesVertices |> Option.defaultValue []
                    faces = frameJson.edgesFaces |> Option.defaultValue []
                    assignment =
                        frameJson.edgesAssignment
                        |> Option.defaultValue []
                    foldAngle = frameJson.edgesFoldAngle |> Option.defaultValue []
                    length = frameJson.edgesLength |> Option.defaultValue []
                    orders = frameJson.edgeOrders |> Option.defaultValue [] }
          faces =
              Faces.Create
                  { vertices = frameJson.facesVertices |> Option.defaultValue []
                    edges = frameJson.facesEdges |> Option.defaultValue []
                    orders = frameJson.faceOrders |> Option.defaultValue [] } }


    let private jsonConfig =
        JsonConfig.create (jsonFieldNaming = nameConversion, serializeNone = SerializeNone.Omit)

    let private jsonConfigUnformatted =
        JsonConfig.create (jsonFieldNaming = nameConversion, serializeNone = SerializeNone.Omit, unformatted = true)

    let ToJson (frame: Frame) : string =
        Json.serializeEx jsonConfig (toJsonType frame)

    let ToJsonUnformatted (frame: Frame) : string =
        Json.serializeEx jsonConfigUnformatted (toJsonType frame)

    let FromJson json : Frame =
        Json.deserializeEx<FrameJson> jsonConfig json
        |> fromJsonType
