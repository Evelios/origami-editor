namespace CreasePattern

type EdgeAssignment =
    | Boundary
    | Mountain
    | Valley
    | Flat
    | Unassigned

type LengthUnit =
    | Meters
    | Pixels
    | Unitless
