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

module FrameJson =
    let nameConversion  = function
        | "edgesFoldAngle" -> "edges_foldAngle"
        | "faceOrders" -> "faceOrders"
        | "edgeOrders" -> "edgeOrders"
        | name -> Json.snakeCase name


    /// Convert the frame type to a json serializable type
    let toJsonType (frame: Frame): FrameJson =
        let stringWithDefault =
            function
            | "" -> None
            | str -> Some str

        let (|EmptySet|_|) a = if Set.isEmpty a then Some() else None

        let setWithDefault =
            function
            | EmptySet -> None
            | set -> Some(Set.toList set)

        { frameAuthor = stringWithDefault frame.author
          frameTitle = stringWithDefault frame.title
          frameDescription = stringWithDefault frame.description
          frameClasses = setWithDefault frame.classes
          frameAttributes = setWithDefault frame.attributes
          frameUnit = Some frame.unit
          verticesCoords = frame.vertices.coords
          verticesVertices = frame.vertices.vertices
          verticesFaces = frame.vertices.faces
          edgesVertices = frame.edges.vertices
          edgesFaces = frame.edges.faces
          edgesAssignment = frame.edges.assignment
          edgesFoldAngle = frame.edges.foldAngle
          edgesLength = frame.edges.length
          edgeOrders = frame.edges.orders
          facesVertices = frame.faces.vertices
          facesEdges = frame.faces.edges
          faceOrders = frame.faces.orders }

    /// Convert the json serializable type to the frame type
    let fromJsonType (frameJson: FrameJson): Frame =
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
                  { coords = frameJson.verticesCoords
                    vertices = frameJson.verticesVertices
                    faces = frameJson.verticesFaces }
          edges =
              Edges.Create
                  { vertices = frameJson.edgesVertices
                    faces = frameJson.edgesFaces
                    assignment = frameJson.edgesAssignment
                    foldAngle = frameJson.edgesFoldAngle
                    length = frameJson.edgesLength
                    orders = frameJson.edgeOrders }
          faces =
              Faces.Create
                  { vertices = frameJson.facesVertices
                    edges = frameJson.facesEdges
                    orders = frameJson.faceOrders } }


    let private jsonConfig =
        JsonConfig.create (jsonFieldNaming = nameConversion, serializeNone = SerializeNone.Omit)

    let private jsonConfigUnformatted =
        JsonConfig.create (jsonFieldNaming = nameConversion, serializeNone = SerializeNone.Omit, unformatted = true)

    let ToJson (frame: Frame): string =
        Json.serializeEx jsonConfig (toJsonType frame)

    let ToJsonUnformatted (frame: Frame): string =
        Json.serializeEx jsonConfigUnformatted (toJsonType frame)

    let FromJson json: Frame =
        Json.deserializeEx<FrameJson> jsonConfig json
        |> fromJsonType
