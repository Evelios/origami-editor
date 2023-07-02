namespace Gui

open Avalonia
open Math.Geometry
open Math.Units
open Origami

type Translation =
    { creasePatternSize: Size2D<Meters>
      pageSize: Size2D<Meters>
      xRatio: float
      yRatio: float }

module Translation =
    (* Builder *)

    let none =
        { creasePatternSize = Size2D.empty
          pageSize = Size2D.empty
          yRatio = 1.
          xRatio = 1. }

    let create creasePatternSize maxPageLength : Translation =
        let pageSize = Size2D.withMaxSize maxPageLength creasePatternSize

        { creasePatternSize = creasePatternSize
          pageSize = pageSize
          yRatio = pageSize.Height / creasePatternSize.Height
          xRatio = pageSize.Width / creasePatternSize.Width }


    (* Conversions *)

    // TODO: check that these conversions are correct
    let vertexToPoint (translation: Translation) (vertex: Point2D<Meters, OrigamiCoordinates>) : Point =
        Point(Length.inCssPixels vertex.X * translation.xRatio, Length.inCssPixels vertex.Y * translation.yRatio)

    // TODO: check that these conversions are correct
    let pointToVertex (translation: Translation) (point: Point) : Point2D<Meters, OrigamiCoordinates> =
        Point2D.xy (point.X / translation.xRatio |> Length.cssPixels) (point.Y / translation.yRatio |> Length.cssPixels)

    let positionToVertex
        (translation: Translation)
        (point: Point2D<Meters, OrigamiCoordinates>)
        : Point2D<Meters, OrigamiCoordinates> =
        Point2D.xy (point.X / translation.xRatio) (point.Y / translation.yRatio)
