namespace Geometry

type Circle2D = { Center: Point2D; Radius: float }

module Circle2D =

    (* Builders *)
    let from c r = { Center = c; Radius = r }

    (* Queries *)
    let isTangentPoint p c =
        almostEqual (Vector2D.magnitude (c.Center - p)) c.Radius
