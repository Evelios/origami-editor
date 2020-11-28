module Util.BoundingBox2d exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Data.AspectRatio as AspectRatio exposing (AspectRatio)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Quantity.Interval as Interval exposing (Interval)
import Vector2d


{-| -}
dimensions :
    BoundingBox2d units coordinates
    -> ( Quantity Float units, Quantity Float units )
dimensions boundingBox =
    BoundingBox2d.intervals boundingBox
        |> Tuple.mapBoth Interval.width Interval.width


{-| -}
edges : BoundingBox2d units coordinates -> List (LineSegment2d units coordinates)
edges boundingBox =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema boundingBox

        { bl, tl, tr, br } =
            { bl = Point2d.xy minX minY
            , tl = Point2d.xy minX maxY
            , tr = Point2d.xy maxX maxY
            , br = Point2d.xy maxX minY
            }
    in
    [ LineSegment2d.from bl tl
    , LineSegment2d.from tl tr
    , LineSegment2d.from tr br
    , LineSegment2d.from br bl
    ]


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
shrinkToAspectRatio :
    AspectRatio
    -> BoundingBox2d units coordinates
    -> BoundingBox2d units coordinates
shrinkToAspectRatio newRatio boundingBox =
    let
        ( width, height ) =
            dimensions boundingBox

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


{-| -}
withTopLeft :
    Point2d units coordinates
    -> BoundingBox2d units coordinates
    -> BoundingBox2d units coordinates
withTopLeft topLeft boundingBox =
    let
        ( width, height ) =
            dimensions boundingBox

        center =
            topLeft
                |> Point2d.translateBy
                    (Vector2d.xy (Quantity.half width) (Quantity.half height))
    in
    BoundingBox2d.withDimensions ( width, height ) center
