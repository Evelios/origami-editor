module Framework.View exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Data.AspectRatio as AspectRatio
import Element exposing (..)
import Element.Background as Background
import Framework.Page as Page
import Html.Attributes
import Pixels exposing (Pixels)
import Util.BoundingBox2d as BoundingBox2d


page : BoundingBox2d Pixels coordinates -> Element msg
page boundingBox =
    let
        paperSize =
            BoundingBox2d.shrinkToAspectRatio
                (AspectRatio.unsafe 1 1)
                boundingBox
    in
    el
        [ width fill
        , height fill
        , htmlAttribute <| Html.Attributes.id pageId
        , Background.color <| rgb255 200 200 200
        ]
        (Page.view paperSize)


pageId : String
pageId =
    "page"
