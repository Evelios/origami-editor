namespace Geometry

open MathNet.Spatial

type Line2D = Line2D of Euclidean.Line2D


module Line2D =

    let fromTo (Point2D start) (Point2D finish) = Euclidean.Line2D(start, finish) |> Line2D

    let pointOnLine (Point2D point) (Line2D line) =
        point = line.StartPoint
        || point = line.EndPoint
        || line
            .ClosestPointTo(point, false)
            .Equals(point, Generics.Epsilon)
