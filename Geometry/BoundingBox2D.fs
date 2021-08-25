namespace Geometry

[<Struct>]
type BoundingBox2D =
    private
        { minX: float
          maxX: float
          maxY: float
          minY: float }

    member this.MinX = this.minX
    member this.MaxX = this.maxX
    member this.MinY = this.minY
    member this.MaxY = this.maxY
    member this.TopLeft = Point2D.xy this.minX this.maxY
    member this.TopRight = Point2D.xy this.maxX this.maxY
    member this.BottomRight = Point2D.xy this.maxX this.minY
    member this.BottomLeft = Point2D.xy this.minX this.minY

module BoundingBox2D =
    (* Builders *)

    // Creates an infinitely small bounding box. This can be used when growing a bounding box around objects
    let empty =
        { minX = infinity
          maxX = -infinity
          minY = infinity
          maxY = -infinity }

    /// Create a bounding box that contains the two points
    let from (p1: Point2D) (p2: Point2D) =
        { minX = min p1.x p2.x
          maxX = max p1.x p2.x
          minY = min p1.y p2.y
          maxY = max p1.y p2.y }

    (* Accessors *)

    /// Returned in clockwise order from top left rotating around clockwise
    let corners (bbox: BoundingBox2D) : Point2D list =
        [ bbox.TopLeft
          bbox.TopRight
          bbox.BottomRight
          bbox.BottomLeft ]

    let width (bbox: BoundingBox2D) : float = bbox.MaxX - bbox.MinX

    let height (bbox: BoundingBox2D) : float = bbox.MaxY - bbox.MinY

    (* Modifiers *)

    /// Get a bounding box that contains the new point. If the box does not contain the new point, the box will grow
    /// to fit the new point. If the point is within the box, the same bounding box is returned.
    let containingPoint (point: Point2D) box =
        { minX = min box.minX point.x
          maxX = max box.maxX point.x
          minY = min box.minY point.y
          maxY = max box.maxY point.y }

    let containingPoints points box =
        Seq.fold (fun box point -> containingPoint point box) box points

    (* Queries *)

    /// Get the four line segments surrounding the bounding box. The lines are created from the top left point, creating
    /// line segments around the bounding box clockwise.
    let lineSegments (bbox: BoundingBox2D) =
        [ LineSegment2D.from bbox.TopLeft bbox.TopRight
          LineSegment2D.from bbox.TopRight bbox.BottomRight
          LineSegment2D.from bbox.BottomRight bbox.BottomLeft
          LineSegment2D.from bbox.BottomLeft bbox.TopLeft ]
