namespace Geometry

type Triangle2D =
    { P1: Point2D
      P2: Point2D
      P3: Point2D }

module Triangle2D =

    (* Builders *)

    let from p1 p2 p3 = { P1 = p1; P2 = p2; P3 = p3 }


    (* Accessors *)

    let boundingBox t =
        BoundingBox2D.surroundingPoints [ t.P1
                                          t.P2
                                          t.P3 ]


    (* Modifiers *)

    let translate v t =
        { P1 = t.P1 + v
          P2 = t.P2 + v
          P3 = t.P3 + v }
