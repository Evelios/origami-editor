module Geometry.Tolerance exposing (float, quantity, angle)

{-|


# Tolerances

@docs float, quantity, angle

-}

import Angle exposing (Angle)
import Quantity exposing (Quantity)


{-| -}
float : Float
float =
    1.0e-12


{-| -}
quantity : Quantity Float units
quantity =
    Quantity.unsafe float


{-| -}
angle : Angle
angle =
    Angle.degrees <| float / 360
