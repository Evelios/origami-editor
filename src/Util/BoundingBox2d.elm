module Util.BoundingBox2d exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Data.AspectRatio as AspectRatio exposing (AspectRatio)
import Data.Coordinates exposing (Cartesian)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)
import Quantity.Interval as Interval exposing (Interval)


{-| Get the aspect ratio of a bounding box.
-}
aspectRatio : BoundingBox2d units coordinates -> AspectRatio
aspectRatio boundingBox =
    case BoundingBox2d.intervals boundingBox of
        ( width, height ) ->
            AspectRatio.from width height


{-| Change the aspect ratio of the current size but make sure that the new size remains in the same bounding box as the
original size object. The bounding box is shrunk around it's center point.
-}
shrinkToAspectRatio : AspectRatio -> BoundingBox2d Pixels Cartesian -> BoundingBox2d Pixels Cartesian
shrinkToAspectRatio newRatio boundingBox =
    let
        ( width, height ) =
            BoundingBox2d.intervals boundingBox
                |> Tuple.mapBoth Interval.width Interval.width

        centerPoint =
            BoundingBox2d.centerPoint boundingBox

        maxDimension =
            Quantity.max width height

        unscaledBoundingBox =
            BoundingBox2d.withDimensions
                ( Quantity.multiplyBy (AspectRatio.xNormalizedBelowOne newRatio) maxDimension
                , Quantity.multiplyBy (AspectRatio.yNormalizedBelowOne newRatio) maxDimension
                )
                centerPoint

        ( unscaledWidth, unscaledHeight ) =
            BoundingBox2d.intervals unscaledBoundingBox
                |> Tuple.mapBoth Interval.width Interval.width

        scaleRatio =
            min
                (Quantity.ratio width unscaledWidth)
                (Quantity.ratio height unscaledHeight)
    in
    BoundingBox2d.scaleAbout centerPoint scaleRatio unscaledBoundingBox
