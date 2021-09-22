module WebApplication.Demos.Draw

open Feliz
open Geometry

let private theme =
    {| line = {| width = 3.; color = "#000000" |}
       point = {| radius = 5.; color = "#000000" |}
       boundingBox = {| color = "#EEEEEE" |} |}

let point (p: Point2D) attr =
    Svg.circle (
        [ svg.cx p.X
          svg.cy p.Y
          svg.r theme.point.radius
          svg.fill theme.point.color ]
        @ attr
    )

let lineSegment (l: LineSegment2D) attr =
    Svg.line (
        [ svg.x1 l.Start.X
          svg.y1 l.Start.Y
          svg.x2 l.Finish.X
          svg.y2 l.Finish.Y
          svg.strokeWidth theme.line.width
          svg.stroke theme.line.color ]
        @ attr
    )

let line (l: Line2D) (bbox: BoundingBox2D) attr =
    (Boolean2D.boundingBoxAndLine bbox l)
    |> Option.map (fun seg -> lineSegment seg attr)

let triangle (t: Triangle2D) attr =
    Svg.polygon (
        [ svg.points [ t.P1.X, t.P1.Y
                       t.P2.X, t.P2.Y
                       t.P3.X, t.P3.Y ]
          svg.strokeWidth theme.line.width
          svg.stroke theme.line.color
          svg.fill "none" ]
        @ attr
    )

let circle (c: Circle2D) attr =
    Svg.circle (
        [ svg.cx c.Center.X
          svg.cy c.Center.Y
          svg.r c.Radius
          svg.fill "none"
          svg.strokeWidth theme.line.width
          svg.stroke theme.line.color ]
        @ attr
    )

let boundingBox (bbox: BoundingBox2D) attr =
    Svg.rect (
        [ svg.x bbox.TopLeft.X
          svg.y bbox.TopLeft.Y
          svg.width (BoundingBox2D.width bbox)
          svg.height (BoundingBox2D.height bbox)
          svg.fill "none"
          svg.strokeWidth theme.line.width
          svg.stroke theme.line.color ]
        @ attr
    )
