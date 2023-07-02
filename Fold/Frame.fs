namespace Fold

open FSharp.Json
open Math.Geometry
open Math.Units

type FrameClass =
    | [<JsonUnionCase("creasePattern")>] CreasePattern
    | [<JsonUnionCase("foldedForm")>] FoldedForm
    | [<JsonUnionCase("graph")>] Graph
    | [<JsonUnionCase("linkage")>] Linkage

type FrameAttribute =
    | [<JsonUnionCase("2D")>] Geo2D
    | [<JsonUnionCase("3D")>] Geo3D
    | [<JsonUnionCase("abstract")>] Abstract
    | [<JsonUnionCase("orientable")>] Orientable
    | [<JsonUnionCase("manifold")>] Manifold
    | [<JsonUnionCase("nonManifold")>] NonManifold
    | [<JsonUnionCase("nonOrientable")>] NonOrientable
    | [<JsonUnionCase("selfTouching")>] SelfTouching
    | [<JsonUnionCase("nonSelfTouching")>] NonSelfTouching
    | [<JsonUnionCase("nonSelfIntersecting")>] NonSelfIntersecting

type LengthUnit =
    | [<JsonUnionCase("unit")>] Unitless
    | [<JsonUnionCase("in")>] Inches
    | [<JsonUnionCase("pt")>] Points
    | [<JsonUnionCase("m")>] Meters
    | [<JsonUnionCase("cm")>] Centimeters
    | [<JsonUnionCase("mm")>] Millimeters
    | [<JsonUnionCase("um")>] Micrometers
    | [<JsonUnionCase("nm")>] Nanometers
    
type FrameJson<'Coordinates> =
    { FrameAuthor: string option
      FrameTitle: string option
      FrameDescription: string option
      FrameClasses: FrameClass list option
      FrameAttributes: FrameAttribute list option
      FrameUnit: LengthUnit option
      [<JsonField(Transform = typeof<Point2D.ListTransform>)>]
      VerticesCoords: Point2D<Meters, 'Coordinates> list option
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

type Frame<'Coordinates> =
    { Author: string
      Title: string
      Description: string
      Classes: FrameClass Set
      Attributes: FrameAttribute Set
      Unit: LengthUnit
      Vertices: Vertices<'Coordinates>
      Edges: Edges
      Faces: Faces }

module Frame =

    let create a : Frame<'Coordinates> = a


    let empty : Frame<'Coordinates> =
        { Author = ""
          Title = ""
          Description = ""
          Classes = Set.empty
          Attributes = Set.empty
          Unit = LengthUnit.Unitless
          Vertices = Vertices.empty
          Edges = Edges.empty
          Faces = Faces.empty }

    (* Modifiers *)

    let setAuthor author frame : Frame<'Coordinates> = { frame with Author = author }

    let setTitle title frame : Frame<'Coordinates> = { frame with Title = title }

    let setDescription description frame : Frame<'Coordinates> =
        { frame with Description = description }

    let setClasses classes frame : Frame<'Coordinates> = { frame with Classes = classes }

    let addClass theClass frame : Frame<'Coordinates> =
        { frame with
              Classes = Set.add theClass frame.Classes }

    let removeClass theClass frame : Frame<'Coordinates> =
        { frame with
              Classes = Set.remove theClass frame.Classes }

    let withoutClasses frame : Frame<'Coordinates> = { frame with Classes = Set.empty }
    let setAttributes attributes frame : Frame<'Coordinates> = { frame with Attributes = attributes }

    let addAttribute attribute frame : Frame<'Coordinates> =
        { frame with
              Attributes = Set.add attribute frame.Attributes }

    let removeAttribute attribute frame : Frame<'Coordinates> =
        { frame with
              Attributes = Set.remove attribute frame.Attributes }

    let withoutAttributes frame : Frame<'Coordinates> = { frame with Attributes = Set.empty }
    let setUnit unit frame : Frame<'Coordinates> = { frame with Unit = unit }

    let setVertices vertices frame : Frame<'Coordinates> = { frame with Vertices = vertices }
    let setEdges edges frame : Frame<'Coordinates> = { frame with Edges = edges }
    let setFaces faces frame : Frame<'Coordinates> = { frame with Faces = faces }

    (* Json Serialization & Deserialization *)

    let internal nameConversion =
        function
        | "EdgesFoldAngle" -> "edges_foldAngle"
        | "FaceOrders" -> "faceOrders"
        | "EdgeOrders" -> "edgeOrders"
        | name -> Json.snakeCase name

    /// Convert the frame type to a json serializable type
    let internal toJsonType (frame: Frame<'Coordinates>) : FrameJson<'Coordinates> =
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

        { FrameAuthor = stringWithDefault frame.Author
          FrameTitle = stringWithDefault frame.Title
          FrameDescription = stringWithDefault frame.Description
          FrameClasses = setWithDefault frame.Classes
          FrameAttributes = setWithDefault frame.Attributes
          FrameUnit = Some frame.Unit
          VerticesCoords = frame.Vertices.Coordinates |> listWithDefault
          VerticesVertices = frame.Vertices.Vertices |> listWithDefault
          VerticesFaces = frame.Vertices.Faces |> listWithDefault
          EdgesVertices = frame.Edges.Vertices |> listWithDefault
          EdgesFaces = frame.Edges.Faces |> listWithDefault
          EdgesAssignment = frame.Edges.Assignment |> listWithDefault
          EdgesFoldAngle = frame.Edges.FoldAngle |> listWithDefault
          EdgesLength = frame.Edges.Length |> listWithDefault
          EdgeOrders = frame.Edges.Orders |> listWithDefault
          FacesVertices = frame.Faces.Vertices |> listWithDefault
          FacesEdges = frame.Faces.Edges |> listWithDefault
          FaceOrders = frame.Faces.Orders |> listWithDefault }

    /// Convert the json serializable type to the frame type
    let internal fromJsonType (frameJson: FrameJson<'Coordinates>) : Frame<'Coordinates> =
        let orEmptyString = Option.defaultValue ""

        let toSet =
            function
            | None -> Set.empty
            | Some list -> Set.ofList list

        { Author = orEmptyString frameJson.FrameAuthor
          Title = orEmptyString frameJson.FrameTitle
          Description = orEmptyString frameJson.FrameDescription
          Classes = frameJson.FrameClasses |> toSet
          Attributes = frameJson.FrameAttributes |> toSet
          Unit =
              frameJson.FrameUnit
              |> Option.defaultValue LengthUnit.Unitless
          Vertices =
              Vertices.create
                  { Coordinates = frameJson.VerticesCoords |> Option.defaultValue []
                    Vertices =
                        frameJson.VerticesVertices
                        |> Option.defaultValue []
                    Faces = frameJson.VerticesFaces |> Option.defaultValue [] }
          Edges =
              Edges.create
                  { Vertices = frameJson.EdgesVertices |> Option.defaultValue []
                    Faces = frameJson.EdgesFaces |> Option.defaultValue []
                    Assignment =
                        frameJson.EdgesAssignment
                        |> Option.defaultValue []
                    FoldAngle = frameJson.EdgesFoldAngle |> Option.defaultValue []
                    Length = frameJson.EdgesLength |> Option.defaultValue []
                    Orders = frameJson.EdgeOrders |> Option.defaultValue [] }
          Faces =
              Faces.create
                  { Vertices = frameJson.FacesVertices |> Option.defaultValue []
                    Edges = frameJson.FacesEdges |> Option.defaultValue []
                    Orders = frameJson.FaceOrders |> Option.defaultValue [] } }


    let private jsonConfig =
        JsonConfig.create (jsonFieldNaming = nameConversion, serializeNone = SerializeNone.Omit)

    let private jsonConfigUnformatted =
        JsonConfig.create (jsonFieldNaming = nameConversion, serializeNone = SerializeNone.Omit, unformatted = true)

    let toJson (frame: Frame<'Coordinates>) : string =
        Json.serializeEx jsonConfig (toJsonType frame)

    let toJsonUnformatted (frame: Frame<'Coordinates>) : string =
        Json.serializeEx jsonConfigUnformatted (toJsonType frame)

    let fromJson<'Coordinates> json : Frame<'Coordinates> =
        Json.deserializeEx<FrameJson<'Coordinates>> jsonConfig json
        |> fromJsonType
