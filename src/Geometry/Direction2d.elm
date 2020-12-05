module Geometry.Direction2d exposing (..)

{-| -}

import Angle
import Direction2d exposing (Direction2d)
import Quantity


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
