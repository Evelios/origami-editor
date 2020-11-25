module Util.Geometry exposing (..)

{-| -}

import Quantity exposing (Quantity)


{-| -}
tolerance : Quantity Float units
tolerance =
    Quantity.unsafe 1.0e-12
