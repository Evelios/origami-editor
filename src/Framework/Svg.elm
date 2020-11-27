module Framework.Svg exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Data.Coordinates exposing (Cartesian, SvgYUp)
import Pixels exposing (Pixels)
import Quantity
import Quantity.Interval as Interval
import Svg exposing (Attribute)
import TypedSvg.Attributes as Attributes
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Types exposing (Transform(..))


{-| Create the attributes necessary to properly render an Svg image while using
cartesian coordinates.
-}
boundingBoxAttributes : BoundingBox2d units Cartesian -> BoundingBox2d Pixels SvgYUp -> List (Attribute msg)
boundingBoxAttributes pageBoundingBox viewBoundingBox =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema pageBoundingBox

        ( width, height ) =
            BoundingBox2d.intervals viewBoundingBox
                |> Tuple.mapBoth Interval.width Interval.width
    in
    [ Attributes.viewBox
        (Quantity.unwrap minX)
        (Quantity.unwrap minY)
        (Quantity.unwrap (maxX |> Quantity.minus minX))
        (Quantity.unwrap (maxY |> Quantity.minus minY))
    , InPx.width (Pixels.inPixels width)
    , InPx.height (Pixels.inPixels height)
    , Attributes.transform [ Scale 1 -1 ]
    ]
