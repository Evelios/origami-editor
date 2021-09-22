namespace Geometry

module Intersection2D =
    open Utilities.Extensions

    /// Try to find the intersection between a line segment and a line. If the lines are parallel (even if they are
    /// overlapping) then no intersection is returned.
    let lineSegmentAndLine (first: LineSegment2D) (second: Line2D) : Point2D option =
        let areParallel =
            let d1 = LineSegment2D.direction first
            let d2 = Line2D.direction second
            d1 = d2 || Vector2D.neg d1 = d2

        if areParallel then
            None
        else
            // http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
            let p = first.start
            let q = second.start
            let r = first.finish - first.start
            let s = second.finish - second.start

            let t =
                Vector2D.crossProduct (q - p) s
                / Vector2D.crossProduct r s

            if (0.0 <= t && t <= 1.0) then
                p + (t * r) |> Some
            else
                None

    let lineAndLineSegment line segment = lineSegmentAndLine segment line

    /// Get all the intersection points between a bounding box and a line
    let boundingBoxAndLine bbox line : Point2D list =
        BoundingBox2D.lineSegments bbox
        |> List.filterMap (lineAndLineSegment line)
        |> List.distinct

    /// A note on angle convention:
    ///
    ///                            +y
    ///
    ///                            |    /
    ///                            |   /
    ///                            |  /   +ve Theta
    ///                            | / )
    /// ±180 degrees    -x ----------------- +ve x    0 degrees
    ///                            | \ )
    ///                            |  \   -ve Theta
    ///                            |   \
    ///                            |    \
    ///
    ///                           -y
    ///
    /// The outputs are ordered from the angle of the lines drawn from the intersection
    /// points to the centre of the first circle, from +180 degrees to -180 degrees.
    //
    // Code and algorithm taken from
    //     http://paulbourke.net/geometry/circlesphere/
    let circles c0 c1 : Point2D seq =
        match Vector2D.magnitude (c0.Center - c1.Center) with
        | d when
            // Circles are disjoint
            d > c0.Radius + c1.Radius
            // Once circle contains the other
            || d < abs (c1.Radius - c0.Radius)
            // Circles are equal and coincident so they have infinite solutions
            || d = 0. && c0.Radius = c1.Radius -> Seq.empty

        // Circles are tangent and have one solution
        | d when d = c0.Radius + c1.Radius ->
            Point2D.lerp (c0.Radius / (c0.Radius + c1.Radius)) c0.Center c1.Center
            |> Seq.singleton

        | d ->
            // Pre-calculations for easier readability
            let p0, p1 = c0.Center, c1.Center
            let r0_2, r1_2 = c0.Radius ** 2., c1.Radius ** 2.
            let dp = p1 - p0
            let dx, dy = dp.X, dp.Y

            // Intermediary values
            let chordDistance = (r0_2 - r1_2 + d ** 2.) / (2. * d)
            let halfChordLength = sqrt (r0_2 - chordDistance ** 2.)
            let chordMidpoint = p0 + ((chordDistance * dp) / d)

            // Intersection points
            let intersection1 =
                Point2D.xy (chordMidpoint.X + halfChordLength * dy / d) (chordMidpoint.Y - halfChordLength * dx / d)

            let theta1 =
                Angle.atan2 (intersection1.Y - p0.y) (intersection1.x - p0.x)

            let intersection2 =
                Point2D.xy (chordMidpoint.X - halfChordLength * dy / d) (chordMidpoint.Y + halfChordLength * dx / d)

            let theta2 =
                Angle.atan2 (intersection2.Y - p0.y) (intersection2.x - p0.x)

            // Intersection sorting
            if theta1 > theta2 then
                Seq.ofList [ intersection1
                             intersection2 ]
            else
                Seq.ofList [ intersection2
                             intersection1 ]
