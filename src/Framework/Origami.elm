module Framework.Origami exposing (..)

{-| -}

import BoundingBox2d exposing (BoundingBox2d)
import Framework.Color
import Framework.Events
import Geometry.Coordinates exposing (SvgYDown, SvgYUp)
import Geometry.Svg as Svg
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Svg exposing (Svg)
import TypedSvg.Attributes as Attributes
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Types exposing (Paint(..))



-- Paper


page :
    List (Svg.Attribute msg)
    ->
        { onClick : Point2d Pixels SvgYDown -> msg
        , onMouseMove : Point2d Pixels SvgYDown -> msg
        , onEnter : Point2d Pixels SvgYDown -> msg
        , onLeave : Point2d Pixels SvgYDown -> msg
        , size : BoundingBox2d Pixels SvgYDown
        }
    -> Svg msg
page attributes options =
    Svg.boundingBox2d
        ([ Attributes.fill <| Paint Framework.Color.paperColor
         , Framework.Events.onClick options.onClick
         , Framework.Events.onMouseMove options.onMouseMove
         , Framework.Events.onEnter options.onEnter
         , Framework.Events.onLeave options.onLeave
         ]
            ++ attributes
        )
        options.size



-- Edges


thickness =
    { thin = 1
    , medium = 2
    , thick = 4
    }


{-| -}
crease : List (Svg.Attribute msg) -> LineSegment2d Pixels SvgYDown -> Svg msg
crease attributes =
    Svg.lineSegment2d
        ([ InPx.strokeWidth thickness.medium
         , Attributes.stroke <| Paint Framework.Color.paperBorder
         ]
            ++ attributes
        )


{-| -}
mountainFold : List (Svg.Attribute msg) -> LineSegment2d Pixels SvgYDown -> Svg msg
mountainFold attributes =
    Svg.lineSegment2d
        ([ InPx.strokeWidth thickness.medium
         , Attributes.stroke <| Paint Framework.Color.paperBorder
         ]
            ++ attributes
        )


{-| -}
valleyFold : List (Svg.Attribute msg) -> LineSegment2d Pixels SvgYDown -> Svg msg
valleyFold attributes =
    Svg.lineSegment2d
        ([ InPx.strokeWidth thickness.medium
         , Attributes.stroke <| Paint Framework.Color.paperBorder
         ]
            ++ attributes
        )


{-| -}
potentialFold : LineSegment2d Pixels SvgYDown -> Svg msg
potentialFold =
    Svg.lineSegment2d
        [ InPx.strokeWidth thickness.thin
        , Attributes.stroke <| Paint Framework.Color.paperBorder
        ]


{-| -}
hoveredPotentialFold : LineSegment2d Pixels SvgYDown -> Svg msg
hoveredPotentialFold =
    Svg.lineSegment2d
        [ InPx.strokeWidth thickness.medium
        , Attributes.stroke <| Paint Framework.Color.paperBorder
        ]
