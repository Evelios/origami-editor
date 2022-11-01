namespace Gui

open Avalonia
open Math.Geometry
open Math.Units

type Translation =
    { creasePatternSize: Size2D<Meters>
      pageSize: Size2D<Meters>
      xRatio: float
      yRatio: float }

module Translation =
    // ---- Builders ---------------------------------------------------------------
    let none<'Coordinates> : Translation =
        { creasePatternSize = Size2D.empty
          pageSize = Size2D.empty
          yRatio = 1.
          xRatio = 1. }

    let create creasePatternSize maxPageLength : Translation =
        let pageSize =
            Size2D.withMaxSize maxPageLength creasePatternSize

        { creasePatternSize = creasePatternSize
          pageSize = pageSize
          yRatio = pageSize.Height / creasePatternSize.Height
          xRatio = pageSize.Width / creasePatternSize.Width }


    // ---- Conversions ------------------------------------------------------------

    let vertexToPoint (translation: Translation) (vertex: Point2D<'Units, 'Coordinates>) : Point =
        Point(vertex.X.Value * translation.xRatio, vertex.Y.Value * translation.yRatio)

    let pointToVertex (translation: Translation) (point: Point) : Point2D<'Units, 'Coordinates> =
        Point2D.xy (Quantity.create (point.X / translation.xRatio)) (Quantity.create (point.Y / translation.yRatio))

    let positionToVertex
        (translation: Translation)
        (point: Point2D<'Units, 'Coordinates>)
        : Point2D<'Units, 'Coordinates> =
        Point2D.xy (point.X / translation.xRatio) (point.Y / translation.yRatio)
