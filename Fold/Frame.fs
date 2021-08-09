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

type LengthUnit =
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
      unit: LengthUnit
      vertices: Vertices
      edges: Edges
      faces: Faces }

module Frame =

    let create a : Frame = a

    let empty : Frame =
        { author = ""
          title = ""
          description = ""
          classes = Set.empty
          attributes = Set.empty
          unit = LengthUnit.Unitless
          vertices = Vertices.empty
          edges = Edges.empty
          faces = Faces.empty }

    (* Modifiers *)

    let setAuthor author frame : Frame = { frame with author = author }

    let setTitle title frame : Frame = { frame with title = title }

    let setDescription description frame : Frame =
        { frame with description = description }

    let setClasses classes frame : Frame = { frame with classes = classes }

    let addClass theClass frame : Frame =
        { frame with
              classes = Set.add theClass frame.classes }

    let removeClass theClass frame : Frame =
        { frame with
              classes = Set.remove theClass frame.classes }

    let withoutClasses frame : Frame = { frame with classes = Set.empty }
    let setAttributes attributes frame : Frame = { frame with attributes = attributes }

    let addAttribute attribute frame : Frame =
        { frame with
              attributes = Set.add attribute frame.attributes }

    let removeAttribute attribute frame : Frame =
        { frame with
              attributes = Set.remove attribute frame.attributes }

    let withoutAttributes frame : Frame = { frame with attributes = Set.empty }
    let setUnit unit frame : Frame = { frame with unit = unit }

    let setVertices vertices frame : Frame = { frame with vertices = vertices }
    let setEdges edges frame : Frame = { frame with edges = edges }
    let setFaces faces frame : Frame = { frame with faces = faces }
