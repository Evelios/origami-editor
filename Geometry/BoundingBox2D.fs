namespace Geometry

type BoundingBox2D =
    | BoundingBox2D of
        {| minX: float
           maxX: float
           maxY: float
           minY: float |}

    member this.minX =
        match this with
        | BoundingBox2D this -> this.minX

    member this.maxX =
        match this with
        | BoundingBox2D this -> this.maxX

    member this.minY =
        match this with
        | BoundingBox2D this -> this.minY

    member this.maxY =
        match this with
        | BoundingBox2D this -> this.maxY

module BoundingBox2D =

    let empty =
        BoundingBox2D
            {| minX = infinity
               maxX = -infinity
               minY = infinity
               maxY = -infinity |}

    let containingPoint (point: Point2D) (BoundingBox2D box) =
        BoundingBox2D
            {| box with
                   minX = min box.minX point.x
                   maxX = max box.maxX point.x
                   minY = min box.minY point.y
                   maxY = max box.maxY point.y |}

    let intersect (line: Line2D) (BoundingBox2D box) : Point2D list = []
