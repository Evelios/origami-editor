module Geometry.Coordinates exposing
    ( Cartesian, SvgYDown, SvgYUp
    , svgYDownToCartesian, svgYUpToCartesian
    )

{-|


# Coordinate Systems

@docs Cartesian, SvgYDown, SvgYUp


# Conversions

@docs svgYDownToCartesian, svgYUpToCartesian

-}

import BoundingBox2d exposing (BoundingBox2d)
import Frame2d exposing (Frame2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate, Unitless)
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
type SvgYUp
    = SvgYUp


{-| Convert from the on screen system of a Y Down coordinate system in Pixels
to the unitless cartesian coordinate system. The coordinates are shrunk to
make sure that they fit within the unit square in the unitless system. That
means that everything within the given bounding box lies within the range
(-1, 1) and domain of (-1, 1) as well.
-}
svgYDownToCartesian :
    BoundingBox2d Pixels coordinates
    ->
        { frame : Frame2d Pixels SvgYDown { defines : Cartesian }
        , rate : Quantity Float (Rate Pixels Unitless)
        }
svgYDownToCartesian boundingBox =
    let
        ( halfWidth, halfHeight ) =
            BoundingBox2d.intervals boundingBox
                |> Tuple.mapBoth
                    (Quantity.half << Interval.width)
                    (Quantity.half << Interval.width)
    in
    { frame =
        Frame2d.atPoint (Point2d.xy halfWidth halfHeight)
            |> Frame2d.reverseY
    , rate = Quantity.max halfWidth halfHeight |> Quantity.per (Quantity.float 1)
    }


{-| Convert from the on screen system of a Y Down coordinate system in Pixels
to the unitless cartesian coordinate system. The coordinates are shrunk to
make sure that they fit within the unit square in the unitless system. That
means that everything within the given bounding box lies within the range
(-1, 1) and domain of (-1, 1) as well.
-}
svgYUpToCartesian :
    BoundingBox2d Pixels coordinates
    ->
        { frame : Frame2d Pixels SvgYUp { defines : Cartesian }
        , rate : Quantity Float (Rate Pixels Unitless)
        }
svgYUpToCartesian boundingBox =
    let
        ( halfWidth, halfHeight ) =
            BoundingBox2d.intervals boundingBox
                |> Tuple.mapBoth
                    (Quantity.half << Interval.width)
                    (Quantity.half << Interval.width)
    in
    { frame =
        Frame2d.atPoint (Point2d.xy halfWidth halfHeight)
    , rate =
        Quantity.max halfWidth halfHeight |> Quantity.per (Quantity.float 1)
    }
