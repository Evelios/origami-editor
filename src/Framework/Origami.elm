module Framework.Origami exposing (..)

{-| -}

import BoundingBox2d exposing (BoundingBox2d)
import Data.Coordinates exposing (SvgYUp)
import Framework.Color
import Framework.Events
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
        { onClick : Point2d Pixels SvgYUp -> msg
        , size : BoundingBox2d units coordinates
        }
    -> Svg msg
page attributes options =
    Svg.boundingBox2d
        ([ Attributes.fill <| Paint Framework.Color.paperColor
         , Framework.Events.onClick options.onClick
         ]
            ++ attributes
        )
        options.size



-- Edges


foldThickness =
    2


{-| -}
crease : List (Svg.Attribute msg) -> LineSegment2d units coordinates -> Svg msg
crease attributes =
    Svg.lineSegment2d
        ([ InPx.width foldThickness
         , Attributes.stroke <| Paint Framework.Color.paperBorder
         ]
            ++ attributes
        )


{-| -}
mountainFold : List (Svg.Attribute msg) -> LineSegment2d units coordinates -> Svg msg
mountainFold attributes =
    Svg.lineSegment2d
        ([ InPx.width foldThickness
         , Attributes.stroke <| Paint Framework.Color.paperBorder
         ]
            ++ attributes
        )


{-| -}
valleyFold : List (Svg.Attribute msg) -> LineSegment2d units coordinates -> Svg msg
valleyFold attributes =
    Svg.lineSegment2d
        ([ InPx.width foldThickness
         , Attributes.stroke <| Paint Framework.Color.paperBorder
         ]
            ++ attributes
        )


{-| -}
potentialFold : LineSegment2d units coordinates -> Svg msg
potentialFold =
    Svg.lineSegment2d
        [ InPx.width foldThickness
        , Attributes.stroke <| Paint Framework.Color.paperBorder
        ]
