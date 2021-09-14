namespace Origami

type EdgeAssignment =
    | Boundary
    | Mountain
    | Valley
    | Flat
    | Preview
    | Unassigned

type LengthUnit =
    | Meters
    | Pixels
    | Unitless
