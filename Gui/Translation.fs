namespace Gui

open Avalonia
open Fold
open Utilities.Collections

type Translation =
    { creasePatternSize: Size
      pageSize: Size
      xRatio: float
      yRatio: float }

module Translation =
    open CreasePattern

    (* Builder *)

    let create creasePattern maxPageLength : Translation =
        let creasePatternSize = CreasePattern.size creasePattern

        let pageSize =
            Size.withMaxSize maxPageLength creasePatternSize

        { creasePatternSize = creasePatternSize
          pageSize = pageSize
          yRatio = pageSize.height / creasePatternSize.height
          xRatio = pageSize.width / creasePatternSize.width }


    (* Conversions *)

    let vertexToPoint (translation: Translation) (vertex: Vertex) : Point =
        Point(Vertex.y vertex * translation.xRatio, Vertex.x vertex * translation.yRatio)

    let pointToVertex (translation: Translation) (point: Point) : Vertex =
        Vertex.in2d (point.X / translation.xRatio) (point.Y / translation.yRatio)
