[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Origami.Fold.LengthUnit

let toString (lengthUnit: LengthUnit) : string =
    match lengthUnit with
    | Unitless -> "unit"
    | Inches -> "in"
    | Points -> "pt"
    | Meters -> "m"
    | Centimeters -> "cm"
    | Millimeters -> "mm"
    | Micrometers -> "um"
    | Nanometers -> "nm"

let fromString (lengthUnit: string) : LengthUnit option =
    match lengthUnit with
    | "unit" -> Some Unitless
    | "in" -> Some Inches
    | "pt" -> Some Points
    | "m" -> Some Meters
    | "cm" -> Some Centimeters
    | "mm" -> Some Millimeters
    | "um" -> Some Micrometers
    | "nm" -> Some Nanometers
    | _ -> None
