module Framework.Svg exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Data.Coordinates exposing (SvgYDown)
import Pixels exposing (Pixels)
import Quantity
import Quantity.Interval as Interval
import Svg exposing (Attribute)
import TypedSvg.Attributes as Attributes
import TypedSvg.Attributes.InPx as InPx


{-| Create the attributes necessary to properly render an Svg image while using
cartesian coordinates.
-}
boundingBoxAttributes : BoundingBox2d Pixels SvgYDown -> List (Attribute msg)
boundingBoxAttributes boundingBox =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema boundingBox

        ( width, height ) =
            BoundingBox2d.intervals boundingBox
                |> Tuple.mapBoth Interval.width Interval.width
    in
    [ Attributes.viewBox
        (Pixels.inPixels minX)
        (Pixels.inPixels minY)
        (Pixels.inPixels (maxX |> Quantity.minus minX))
        (Pixels.inPixels (maxY |> Quantity.minus minY))
    , InPx.width (Pixels.inPixels width)
    , InPx.height (Pixels.inPixels height)
    ]
