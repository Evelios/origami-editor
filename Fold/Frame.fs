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

    let Empty =
        { author = None
          title = None
          description = None
          classes = None
          attributes = None
          unit = None
          vertices = None
          edges = None
          faces = None }


    (* Json *)
    let private jsonConfig =
        JsonConfig.create (jsonFieldNaming = (+) "frame_", serializeNone = SerializeNone.Omit)

    let private jsonConfigUnformatted =
        JsonConfig.create (jsonFieldNaming = (+) "frame_", serializeNone = SerializeNone.Omit, unformatted = true)

    let ToJson (frame: Frame): string = Json.serializeEx jsonConfig frame

    let ToJsonUnformatted (frame: Frame): string =
        Json.serializeEx jsonConfigUnformatted frame

    let FromJson json =
        let frame =
            Json.deserializeEx<Frame> jsonConfig json

        let edges = Edges.FromJson json
        let vertices = Vertices.FromJson json
        let faces = Faces.FromJson json

        let maybe record empty =
            if record = empty then None else Some record

        { frame with
              edges = maybe edges Edges.Empty
              vertices = maybe vertices Vertices.Empty
              faces = maybe faces Faces.Empty }
