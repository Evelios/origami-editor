namespace Fold

open FSharp.Json

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

type Unit =
    | [<JsonUnionCase("unit")>] Unitless
    | [<JsonUnionCase("in")>] Inches
    | [<JsonUnionCase("pt")>] Points
    | [<JsonUnionCase("m")>] Meters
    | [<JsonUnionCase("cm")>] Centimeters
    | [<JsonUnionCase("mm")>] Millimeters
    | [<JsonUnionCase("um")>] Micrometers
    | [<JsonUnionCase("nm")>] Nanometers

type Frame =
    { author: string
      title: string
      description: string
      classes: FrameClass Set
      attributes: FrameAttribute Set
      unit: Unit
      vertices: Vertices
      edges: Edges
      faces: Faces }

type FrameJson =
    { author: string option
      title: string option
      description: string option
      classes: FrameClass list option
      attributes: FrameAttribute list option
      unit: Unit option
      vertices: Vertices option
      edges: Edges option
      faces: Faces option }

module Frame =

    let Create a: Frame = a

    let Empty: Frame =
        { author = ""
          title = ""
          description = ""
          classes = Set.empty
          attributes = Set.empty
          unit = Unit.Unitless
          vertices = Vertices.Empty
          edges = Edges.Empty
          faces = Faces.Empty }

    (* Accessors *)

    let setAuthor author file: Frame = { file with author = author }

    let setTitle title file: Frame = { file with title = title }
    let setDescription description file: Frame = { file with description = description }
    let setClasses classes file: Frame = { file with classes = classes }
    let setAttributes attributes file: Frame = { file with attributes = attributes }
    let setUnit unit file: Frame = { file with unit = unit }


    (* Json *)

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

        let withNoneIf empty value =
            if value = empty then None else Some value

        { author = stringWithDefault frame.author
          title = stringWithDefault frame.title
          description = stringWithDefault frame.description
          classes = setWithDefault frame.classes
          attributes = setWithDefault frame.attributes
          unit = Some frame.unit
          vertices = frame.vertices |> withNoneIf Vertices.Empty
          edges = frame.edges |> withNoneIf Edges.Empty
          faces = frame.faces |> withNoneIf Faces.Empty }

    /// Convert the json serializable type to the frame type
    let fromJsonType (frameJson: FrameJson): Frame =
        let orEmptyString = Option.defaultValue ""

        let toSet =
            function
            | None -> Set.empty
            | Some list -> Set.ofList list

        { author = orEmptyString frameJson.author
          title = orEmptyString frameJson.title
          description = orEmptyString frameJson.description
          classes = frameJson.classes |> toSet
          attributes = frameJson.attributes |> toSet
          unit = Option.defaultValue Unit.Unitless frameJson.unit
          vertices = Option.defaultValue Vertices.Empty frameJson.vertices
          edges = frameJson.edges |> Option.defaultValue Edges.Empty
          faces = frameJson.faces |> Option.defaultValue Faces.Empty }

    let private jsonConfig =
        JsonConfig.create (jsonFieldNaming = (+) "frame_", serializeNone = SerializeNone.Omit)

    let private jsonConfigUnformatted =
        JsonConfig.create (jsonFieldNaming = (+) "frame_", serializeNone = SerializeNone.Omit, unformatted = true)

    let ToJson (frame: Frame): string =
        Json.serializeEx jsonConfig (toJsonType frame)

    let ToJsonUnformatted (frame: Frame): string =
        Json.serializeEx jsonConfigUnformatted (toJsonType frame)

    let FromJson json: Frame =
        let frame =
            Json.deserializeEx<FrameJson> jsonConfig json

        let edges = Edges.FromJson json
        let vertices = Vertices.FromJson json
        let faces = Faces.FromJson json

        { frame with
              edges = Some edges
              vertices = Some vertices
              faces = Some faces }
        |> fromJsonType
