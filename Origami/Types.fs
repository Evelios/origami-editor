namespace Origami

type OrigamiCoordinates = OrigamiCoordinates

[<RequireQualifiedAccess>]
type EdgeAssignment =
    | Boundary
    | Mountain
    | Valley
    | Flat
    | Preview
    | Unassigned

[<RequireQualifiedAccess>]
type LengthUnit =
    | Meters
    | Pixels
    | Unitless
