module Geometry.Direction2d exposing (directionsBetween)

{-|

@docs directionsBetween

-}

import Angle
import Direction2d exposing (Direction2d)
import Quantity


{-| Get the two directions which are between the given directions. The first
direction is the direction clockwise between the first and second directions.
The second is the direction counter-clockwise.
-}
directionsBetween :
    Direction2d coordinates
    -> Direction2d coordinates
    -> ( Direction2d coordinates, Direction2d coordinates )
directionsBetween first second =
    let
        angleBetween =
            Direction2d.angleFrom first second
    in
    ( Direction2d.rotateBy
        (Quantity.half angleBetween)
        first
    , Direction2d.rotateBy
        (Quantity.half angleBetween
            |> Quantity.plus (Angle.radians <| pi / 2)
        )
        first
    )
