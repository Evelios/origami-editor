module Util.Geometry exposing (..)

{-| -}

import Angle exposing (Angle)
import Quantity exposing (Quantity)


tolerance : Float
tolerance =
    1.0e-12


{-| -}
toleranceQuantity : Quantity Float units
toleranceQuantity =
    Quantity.unsafe tolerance


{-| -}
toleranceAngle : Angle
toleranceAngle =
    Angle.degrees <| tolerance / 360
