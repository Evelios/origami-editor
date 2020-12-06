module Framework.Color exposing
    ( paperColor, paperWhite, paperBorder
    , point, pointActive, pointSelected
    )

{-|


# Paper Colors

@docs paperColor, paperWhite, paperBorder


# Points

@docs point, pointActive, pointSelected

-}

import Color exposing (Color)
import Framework.Color.Extra exposing (hexToColor)



-- Colors


{-| -}
paperColor : Color
paperColor =
    hexToColor "#87DEAD"


{-| -}
paperWhite : Color
paperWhite =
    hexToColor "#B5F0CF"


{-| -}
paperBorder : Color
paperBorder =
    hexToColor "#4D4D4D"


{-| -}
point : Color
point =
    hexToColor "#000000"


{-| -}
pointActive : Color
pointActive =
    hexToColor "#1ba3b3"


{-| -}
pointSelected : Color
pointSelected =
    hexToColor "#50c4f2"
