module Framework.Page exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Circle2d
import Color
import Data.Coordinates as Coordinates exposing (Cartesian, SvgYDown)
import Data.Edge exposing (Edge)
import Element exposing (Element, centerX, centerY, html)
import Geometry.Svg as Svg
import Graph exposing (Graph)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Unitless)
import Quantity.Interval as Interval
import Svg exposing (Svg)
import Svg.Events as Events
import TypedSvg.Attributes as Attributes
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Attribute)
import TypedSvg.Types exposing (Paint(..), Transform(..))


colors =
    { background = Color.rgb255 100 100 100
    , vertex = Color.rgb255 180 40 75
    }


pageId : String
pageId =
    "page"



--


vertex : Point2d Pixels coordinates -> Svg msg
vertex point =
    Svg.circle2d
        [ Attributes.fill PaintNone
        , InPx.strokeWidth 2
        , Attributes.stroke <| Paint colors.vertex
        ]
        (Circle2d.atPoint point <| Pixels.pixels 5)


background : BoundingBox2d Pixels coordinates -> Svg msg
background boundingBox =
    Svg.boundingBox2d
        [ Attributes.fill <| Paint colors.background
        ]
        boundingBox


{-| -}
view :
    { graph : Graph (Point2d Pixels Cartesian) Edge
    , boundingBox : BoundingBox2d Pixels Cartesian
    , onClick : Point2d Pixels Cartesian -> msg
    }
    -> Element msg
view { graph, boundingBox, onClick } =
    let
        elements =
            [ background boundingBox ]
                ++ List.map vertex (Graph.vertices graph)
    in
    Svg.svg
        ([ onClickEvent boundingBox <| onClick
         ]
            ++ boundingBoxAttributes boundingBox
        )
        elements
        |> html
        |> Element.el [ centerX, centerY ]
        |> Element.el
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.htmlAttribute <| Html.Attributes.id pageId
            ]



-- Helper Functions


boundingBoxAttributes : BoundingBox2d Pixels Cartesian -> List (Attribute msg)
boundingBoxAttributes boundingBox =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema boundingBox

        ( width, height ) =
            BoundingBox2d.intervals boundingBox
                |> Tuple.mapBoth Interval.width Interval.width
    in
    [ Attributes.viewBox
        (Quantity.unwrap minX)
        (Quantity.unwrap minY)
        (Quantity.unwrap (maxX |> Quantity.minus minX))
        (Quantity.unwrap (maxY |> Quantity.minus minY))
    , InPx.width (Quantity.unwrap width)
    , InPx.height (Quantity.unwrap height)
    , Attributes.transform [ Scale 1 -1 ]
    ]


boundingBoxAttributesCartesian : BoundingBox2d Pixels SvgYDown -> List (Attribute msg)
boundingBoxAttributesCartesian boundingBox =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema boundingBox

        ( width, height ) =
            BoundingBox2d.intervals boundingBox
                |> Tuple.mapBoth Interval.width Interval.width
    in
    [ Attributes.viewBox
        (Quantity.unwrap minX)
        (Quantity.unwrap minY)
        (Quantity.unwrap (maxX |> Quantity.minus minX))
        (Quantity.unwrap (maxY |> Quantity.minus minY))
    , InPx.width (Quantity.unwrap width)
    , InPx.height (Quantity.unwrap height)
    ]


{-| Information about handling mouse events can be found on the [elm package
website][1] and on the [Mozilla documentation][2].

[1]: https://package.elm-lang.org/packages/elm/svg/latest/Svg-Events#on
[2]: https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent

-}
onClickEvent : BoundingBox2d Pixels Cartesian -> (Point2d Pixels Cartesian -> msg) -> Attribute msg
onClickEvent boundingBox message =
    let
        svgDecoder : Decoder (Point2d Pixels SvgYDown)
        svgDecoder =
            Decode.map2
                Point2d.pixels
                (Decode.field "offsetX" Decode.float)
                (Decode.field "offsetY" Decode.float)

        decoder : Decoder (Point2d Pixels Cartesian)
        decoder =
            Decode.map
                (Point2d.relativeTo
                    (Coordinates.svgYDownToCartesian boundingBox)
                )
                svgDecoder
    in
    Events.on "click" (Decode.map message decoder)
