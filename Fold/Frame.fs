namespace Fold

open FSharp.Json


module FrameMetadata =

    type FrameClass =
        | CreasePattern = 0
        | FoldedForm = 1
        | Graph = 2
        | Linkage = 3

    type FrameAttribute =
        | Geo2D = 0
        | Geo3D = 1
        | Abstract = 2
        | Orientable = 3
        | Manifold = 4
        | NonManifold = 5
        | NonOrientable = 6
        | SelfTouching = 7
        | NonSelfTouching = 8
        | NonSelfIntersecting = 9

    type Unit =
        | Unitless = 0
        | Inches = 1
        | Points = 2
        | Meters = 3
        | Centimeters = 4
        | Millimeters = 5
        | Micrometers = 6
        | Nanometers = 7

    let private frameClassConverter =
        StringMap [ FrameClass.CreasePattern, "creasePattern"
                    FrameClass.FoldedForm, "foldedForm"
                    FrameClass.Graph, "graph"
                    FrameClass.Linkage, "linkage" ]

    type FrameClassTransform =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<string>) ()

            member this.fromTargetType text =
                text.ToString() |> frameClassConverter.FromString :> obj

            member this.toTargetType frameClass =
                frameClassConverter.ToString(frameClass :?> FrameClass) :> obj


    let private frameAttributeConverter =
        StringMap [ FrameAttribute.Geo2D, "2D"
                    FrameAttribute.Geo3D, "3D"
                    FrameAttribute.Abstract, "abstract"
                    FrameAttribute.Orientable, "orientable"
                    FrameAttribute.Manifold, "manifold"
                    FrameAttribute.NonManifold, "nonManifold"
                    FrameAttribute.NonOrientable, "nonOrientable"
                    FrameAttribute.SelfTouching, "selfTouching"
                    FrameAttribute.NonSelfTouching, "nonSelfTouching"
                    FrameAttribute.NonSelfIntersecting, "nonSelfIntersecting" ]

    type FrameAttributeTransform =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<string>) ()

            member this.fromTargetType text =
                text.ToString()
                |> frameAttributeConverter.FromString :> obj

            member this.toTargetType frameAttribute =
                frameAttributeConverter.ToString(frameAttribute :?> FrameAttribute) :> obj

    let private unitConverter =
        StringMap [ Unit.Unitless, "unit"
                    Unit.Inches, "in"
                    Unit.Points, "pt"
                    Unit.Meters, "m"
                    Unit.Centimeters, "cm"
                    Unit.Millimeters, "mm"
                    Unit.Micrometers, "um"
                    Unit.Nanometers, "nm" ]

    type UnitTransform =
        interface ITypeTransform with
            member this.targetType() = (fun _ -> typeof<string>) ()

            member this.fromTargetType text =
                text.ToString() |> unitConverter.FromString :> obj

            member this.toTargetType unit =
                unitConverter.ToString(unit :?> Unit) :> obj

type Frame =
    { author: string option
      title: string option
      description: string option
      [<JsonField(Transform = typeof<FrameMetadata.FrameClassTransform>)>]
      classes: FrameMetadata.FrameClass list option
      [<JsonField(Transform = typeof<FrameMetadata.FrameAttributeTransform>)>]
      attributes: FrameMetadata.FrameAttribute list option
      [<JsonField(Transform = typeof<FrameMetadata.UnitTransform>)>]
      unit: FrameMetadata.Unit option }

module Frame =

    let Empty =
        { author = None
          title = None
          description = None
          classes = None
          attributes = None
          unit = None }


    (* Json *)
    let private jsonConfig =
        JsonConfig.create (jsonFieldNaming = (+) "frame_", serializeNone = SerializeNone.Omit)

    let private jsonConfigUnformatted =
        JsonConfig.create (jsonFieldNaming = (+) "frame_", serializeNone = SerializeNone.Omit, unformatted = true)

    let ToJson (fold: Frame): string = Json.serializeEx jsonConfig fold

    let ToJsonUnformatted (frame: Frame): string =
        Json.serializeEx jsonConfigUnformatted frame

    let FromJson = Json.deserializeEx<Frame> jsonConfig
