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

    member this.tl =
        match this with
        | BoundingBox2D this -> Point2D.xy this.minX this.maxY

    member this.tr =
        match this with
        | BoundingBox2D this -> Point2D.xy this.maxX this.maxY

    member this.br =
        match this with
        | BoundingBox2D this -> Point2D.xy this.maxX this.minY

    member this.bl =
        match this with
        | BoundingBox2D this -> Point2D.xy this.minX this.minY

module BoundingBox2D =
    (* Builders *)

    let empty =
        BoundingBox2D
            {| minX = infinity
               maxX = -infinity
               minY = infinity
               maxY = -infinity |}


    (* Modifiers *)

    let containingPoint (point: Point2D) (BoundingBox2D box) =
        BoundingBox2D
            {| box with
                   minX = min box.minX point.x
                   maxX = max box.maxX point.x
                   minY = min box.minY point.y
                   maxY = max box.maxY point.y |}

    (* Queries *)

    let lineSegments (bbox: BoundingBox2D) =
        [ LineSegment2D.from bbox.tl bbox.tr
          LineSegment2D.from bbox.tr bbox.bl
          LineSegment2D.from bbox.bl bbox.br
          LineSegment2D.from bbox.br bbox.tl ]
