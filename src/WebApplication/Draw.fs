module WebApplication.Demos.Draw

open Feliz
open Geometry


let private theme =
    {| line = {| width = 3.; color = "#000000" |}
       point = {| radius = 2.5; color = "#000000" |}
       boundingBox = {| color = "#EEEEEE" |} |}

let point (p: Point2D) =
    Svg.circle [ svg.x p.X
                 svg.y p.Y
                 svg.radius theme.point.radius
                 svg.fill theme.point.color ]

let lineSegment (l: LineSegment2D) =
    Svg.line [ svg.x1 l.Start.X
               svg.y1 l.Start.Y
               svg.x2 l.Finish.X
               svg.y2 l.Finish.Y
               svg.strokeWidth theme.line.width
               svg.stroke theme.line.color ]

let boundingBox (bbox: BoundingBox2D) =
    Svg.rect [ svg.x bbox.TopLeft.X
               svg.y bbox.TopLeft.Y
               svg.width (BoundingBox2D.width bbox)
               svg.height (BoundingBox2D.height bbox)
               svg.fill theme.boundingBox.color
               svg.strokeWidth theme.line.width
               svg.stroke theme.line.color ]
