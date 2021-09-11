namespace Gui

open Avalonia
open Geometry

type Translation =
    { creasePatternSize: Size
      pageSize: Size
      xRatio: float
      yRatio: float }

module Translation =
    (* Builder *)

    let none =
        { creasePatternSize = Size.empty
          pageSize = Size.empty
          yRatio = 1.
          xRatio = 1. }

    let create creasePatternSize maxPageLength : Translation =
        let pageSize =
            Size.withMaxSize maxPageLength creasePatternSize

        { creasePatternSize = creasePatternSize
          pageSize = pageSize
          yRatio = pageSize.Height / creasePatternSize.Height
          xRatio = pageSize.Width / creasePatternSize.Width }


    (* Conversions *)

    let vertexToPoint (translation: Translation) (vertex: Point2D) : Point =
        Point(vertex.X * translation.xRatio, vertex.Y * translation.yRatio)

    let pointToVertex (translation: Translation) (point: Point) : Point2D =
        Point2D.xy (point.X / translation.xRatio) (point.Y / translation.yRatio)

    let positionToVertex (translation: Translation) (point: Point2D) : Point2D =
        Point2D.xy (point.X / translation.xRatio) (point.Y / translation.yRatio)
