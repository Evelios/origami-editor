module Framework.Page exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Circle2d
import Color exposing (Color)
import Data.Coordinates as Coordinates exposing (Cartesian, SvgYDown)
import Data.CreasePattern as CreasePattern exposing (CreasePattern)
import Data.Edge exposing (Edge)
import Element exposing (Element, centerX, centerY, html)
import Framework.Color
import Geometry.Svg as Svg
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import LineSegment2d exposing (LineSegment2d)
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
import Util.List


pageId : String
pageId =
    "page"



--


vertex : Color -> Float -> Point2d Pixels coordinates -> Svg msg
vertex color size point =
    Svg.circle2d
        [ Attributes.fill <| Paint color
        ]
        (Circle2d.atPoint point <| Pixels.pixels size)


vertexStandard : Point2d Pixels coordinates -> Svg msg
vertexStandard =
    vertex Framework.Color.point 3


vertexActive : Point2d Pixels coordinates -> Svg msg
vertexActive =
    vertex Framework.Color.pointActive 3


vertexSelected : Point2d Pixels coordinates -> Svg msg
vertexSelected =
    vertex Framework.Color.pointSelected 5


edge :
    { from : Point2d Pixels coordinates
    , to : Point2d Pixels coordinates
    , data : Edge
    }
    -> Svg msg
edge { from, to } =
    Svg.lineSegment2d
        [ InPx.strokeWidth 2
        , Attributes.stroke <| Paint Framework.Color.paperBorder
        ]
        (LineSegment2d.from from to)


potentialFold :
    LineSegment2d units coordinates
    -> Svg msg
potentialFold line =
    Svg.lineSegment2d
        [ InPx.strokeWidth 1
        , Attributes.stroke <| Paint Framework.Color.paperBorder
        ]
        line


background : BoundingBox2d Pixels coordinates -> Svg msg
background boundingBox =
    Svg.boundingBox2d
        [ Attributes.fill <| Paint Framework.Color.paperColor
        , Attributes.stroke <| Paint Framework.Color.paperColor
        ]
        boundingBox


{-| -}
view :
    { creasePattern : CreasePattern Pixels Cartesian
    , boundingBox : BoundingBox2d Pixels Cartesian
    , hoveredVertex : Maybe (Point2d Pixels Cartesian)
    , selectedVertex : Maybe (Point2d Pixels Cartesian)
    , onMouseMove : Point2d Pixels Cartesian -> msg
    , onMouseDown : Point2d Pixels Cartesian -> msg
    , onMouseUp : Point2d Pixels Cartesian -> msg
    , onMouseLeave : msg
    }
    -> Element msg
view options =
    let
        elements =
            [ background options.boundingBox ]
                ++ List.map vertexStandard (CreasePattern.vertices options.creasePattern)
                ++ List.map edge (CreasePattern.edges options.creasePattern)
                ++ List.map potentialFold (CreasePattern.potentialFolds options.creasePattern)
                |> Util.List.appendIf (Maybe.map vertexActive options.hoveredVertex)
                |> Util.List.appendIf (Maybe.map vertexSelected options.selectedVertex)
    in
    Svg.svg
        ([ onMouseMove options.boundingBox <| options.onMouseMove
         , onMouseDown options.boundingBox <| options.onMouseDown
         , onMouseUp options.boundingBox <| options.onMouseUp
         , Events.onMouseOut options.onMouseLeave
         ]
            ++ boundingBoxAttributes options.boundingBox
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
        (Quantity.unwrap minX - 10)
        (Quantity.unwrap minY - 10)
        (Quantity.unwrap (maxX |> Quantity.minus minX) + 20)
        (Quantity.unwrap (maxY |> Quantity.minus minY) + 20)
    , InPx.width (Quantity.unwrap width)
    , InPx.height (Quantity.unwrap height)
    , Attributes.transform [ Scale 1 -1 ]
    ]



-- Event Handlers


onClickEvent : BoundingBox2d Pixels Cartesian -> (Point2d Pixels Cartesian -> msg) -> Attribute msg
onClickEvent boundingBox message =
    onEvent
        { event = "click"
        , boundingBox = boundingBox
        , onTrigger = message
        }


onMouseDown : BoundingBox2d Pixels Cartesian -> (Point2d Pixels Cartesian -> msg) -> Attribute msg
onMouseDown boundingBox message =
    onEvent
        { event = "mousedown"
        , boundingBox = boundingBox
        , onTrigger = message
        }


onMouseUp : BoundingBox2d Pixels Cartesian -> (Point2d Pixels Cartesian -> msg) -> Attribute msg
onMouseUp boundingBox message =
    onEvent
        { event = "mouseup"
        , boundingBox = boundingBox
        , onTrigger = message
        }


onMouseMove : BoundingBox2d Pixels Cartesian -> (Point2d Pixels Cartesian -> msg) -> Attribute msg
onMouseMove boundingBox message =
    onEvent
        { event = "mousemove"
        , boundingBox = boundingBox
        , onTrigger = message
        }


{-| Information about handling mouse events can be found on the [elm package
website][1] and on the [Mozilla documentation][2].

[1]: https://package.elm-lang.org/packages/elm/svg/latest/Svg-Events#on
[2]: https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent

-}
onEvent :
    { event : String
    , boundingBox : BoundingBox2d Pixels Cartesian
    , onTrigger : Point2d Pixels Cartesian -> msg
    }
    -> Attribute msg
onEvent { event, boundingBox, onTrigger } =
    Events.on event (Decode.map onTrigger <| point2dDecoder boundingBox)


point2dDecoder : BoundingBox2d Pixels Cartesian -> Decoder (Point2d Pixels Cartesian)
point2dDecoder boundingBox =
    let
        svgDecoder : Decoder (Point2d Pixels SvgYDown)
        svgDecoder =
            Decode.map2
                Point2d.pixels
                (Decode.field "offsetX" Decode.float)
                (Decode.field "offsetY" Decode.float)
    in
    Decode.map
        (Point2d.relativeTo
            (Coordinates.svgYDownToCartesian boundingBox)
        )
        svgDecoder
