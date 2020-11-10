module Framework.View exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Data.AspectRatio as AspectRatio
import Element exposing (..)
import Element.Background as Background
import Html.Attributes
import Pixels exposing (Pixels)
import Quantity.Interval as Interval
import Util.BoundingBox2d as BoundingBox2d


page : BoundingBox2d Pixels coordinates -> Element msg
page boundingBox =
    let
        paperSize =
            BoundingBox2d.shrinkToAspectRatio
                (AspectRatio.unsafe 1 1)
                boundingBox

        fillInterval interval =
            interval paperSize
                |> Interval.width
                |> Pixels.inPixels
                |> round
                |> px

        paper =
            el
                [ width <| fillInterval BoundingBox2d.xInterval
                , height <| fillInterval BoundingBox2d.yInterval
                , centerX
                , centerY
                , Background.color <| rgb255 100 100 100
                ]
                none
    in
    el
        [ width fill
        , height fill
        , htmlAttribute <| Html.Attributes.id pageId
        , Background.color <| rgb255 200 200 200
        ]
        paper


pageId : String
pageId =
    "page"
