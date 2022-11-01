[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.Fold.FrameAttribute

let toString (frameAttribute: FrameAttribute) : string =
    match frameAttribute with
    | Geo2D -> "2D"
    | Geo3D -> "3D"
    | Abstract -> "abstract"
    | Orientable -> "orientable"
    | Manifold -> "manifold"
    | NonManifold -> "nonManifold"
    | NonOrientable -> "nonOrientable"
    | SelfTouching -> "selfTouching"
    | NonSelfTouching -> "nonSelfTouching"
    | NonSelfIntersecting -> "nonSelfIntersecting"

let fromString (frameAttribute: string) : FrameAttribute option =
    match frameAttribute with
    | "2D" -> Some Geo2D
    | "3D" -> Some Geo3D
    | "abstract" -> Some Abstract
    | "orientable" -> Some Orientable
    | "manifold" -> Some Manifold
    | "nonManifold" -> Some NonManifold
    | "nonOrientable" -> Some NonOrientable
    | "selfTouching" -> Some SelfTouching
    | "nonSelfTouching" -> Some NonSelfTouching
    | "nonSelfIntersecting" -> Some NonSelfIntersecting
    | _ -> None
