[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.Fold.EdgeAssignment

open Origami

let toString (edgeAssignment: EdgeAssignment) : string =
    match edgeAssignment with
    | Boundary -> "B"
    | Mountain -> "M"
    | Valley -> "V"
    | Flat -> "F"
    | Preview
    | Unassigned -> "U"

let fromString (edgeAssignment: string) : EdgeAssignment =
    match edgeAssignment with
    | "B" -> Boundary
    | "M" -> Mountain
    | "V" -> Valley
    | "F" -> Flat
    | "U" -> Unassigned
    | _ -> Unassigned
