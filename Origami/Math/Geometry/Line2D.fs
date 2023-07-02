module Math.Geometry.Line2D

open Math.Geometry
open Math.Units


/// Get the line that is perpendicular to a given line though the specified point
let perpThroughPoint
    (point: Point2D<Meters, 'Coordinates>)
    (line: Line2D<Meters, 'Coordinates>)
    : Line2D<Meters, 'Coordinates> =

    let perpDirection: Direction2D<'Coordinates> =
        Line2D.direction line
        |> Option.defaultValue Direction2D.x
        |> Direction2D.rotateBy (Angle.degrees 90.)

    let perpPoint = point + Vector2D.meters perpDirection.X perpDirection.Y

    Line2D.through point perpPoint
