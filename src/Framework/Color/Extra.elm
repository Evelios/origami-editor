module Framework.Color.Extra exposing (hexToColor)

{-|


# Convertions to/from Hex

@docs hexToColor

-}

import Color exposing (Color)
import Color.Convert


{-| Converts a string to a color.
-}
hexToColor : String -> Color
hexToColor string =
    let
        cl =
            Color.Convert.hexToColor string
    in
    Result.withDefault (Color.rgb 0 0 0) cl
