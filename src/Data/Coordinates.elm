module Data.Coordinates exposing (Cartesian, SvgYDown)

{-|


# Coordinate Systems

@docs Cartesian, SvgYDown

-}


{-| Cartesian coordinates are centered around the origin (0, 0) and have the
positive Y direction be up and the positive X direction be to the right.
-}
type Cartesian
    = Cartesian


{-| The SVG coordinate system defines the top left corner to be the origin
(0, 0) and for Y to increase downward. In this coordinate system X increases
to the right.
-}
type SvgYDown
    = SvgYDown
