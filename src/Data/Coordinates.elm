module Data.Coordinates exposing
    ( Cartesian, SvgYDown
    , svgYDownToCartesian
    )

{-|


# Coordinate Systems

@docs Cartesian, SvgYDown


# Conversions

@docs svgYDownToCartesian

-}

import BoundingBox2d exposing (BoundingBox2d)
import Frame2d exposing (Frame2d)
import Point2d
import Quantity
import Quantity.Interval as Interval


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


{-| -}
svgYDownToCartesian :
    BoundingBox2d units Cartesian
    -> Frame2d units SvgYDown { defines : Cartesian }
svgYDownToCartesian boundingBox =
    let
        ( width, height ) =
            BoundingBox2d.intervals boundingBox
                |> Tuple.mapBoth Interval.width Interval.width
    in
    Frame2d.atPoint
        (Point2d.xy
            (Quantity.half width)
            (Quantity.half height)
        )
