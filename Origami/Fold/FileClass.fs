[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.Fold.FileClass


let toString (fileClass: FileClass) : string =
    match fileClass with
    | SingleModel -> "singleModel"
    | MultiModel -> "multiModel"
    | Animation -> "animation"
    | Diagrams -> "diagrams"

let fromString (fileClass: string) : FileClass option =
    match fileClass with
    | "singleModel" -> Some SingleModel
    | "multiModel" -> Some MultiModel
    | "animation" -> Some Animation
    | "diagrams" -> Some Diagrams
    | _ -> None
