[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.Fold.FrameClass

let toString (frameClass: FrameClass) : string =
    match frameClass with
    | CreasePattern -> "creasePattern"
    | FoldedForm -> "foldedForm"
    | Graph -> "graph"
    | Linkage -> "linkage"

let fromString (frameClass: string) : FrameClass option =
    match frameClass with
    | "creasePattern" -> Some CreasePattern
    | "foldedForm" -> Some FoldedForm
    | "graph" -> Some Graph
    | "linkage" -> Some Linkage
    | _ -> None

