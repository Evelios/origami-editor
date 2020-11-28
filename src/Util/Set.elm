module Util.Set exposing
    ( Set
    , lineSegment2d, point2d
    )

{-|


# Types

@docs Set


# Sets

@docs lineSegment2d, point2d

-}

import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Round
import Set.Any as Set exposing (AnySet)


type alias Id =
    String


type alias Set geometry =
    AnySet String geometry


{-| -}
lineSegment2d : Set (LineSegment2d units coordinates)
lineSegment2d =
    Set.empty lineToKey


{-| -}
point2d : Set (Point2d units coordinates)
point2d =
    Set.empty pointToKey


lineToKey : LineSegment2d units coordinates -> Id
lineToKey =
    LineSegment2d.endpoints
        >> (\( p1, p2 ) ->
                "["
                    ++ pointToKey p1
                    ++ ", "
                    ++ pointToKey p2
                    ++ "]"
           )


pointToKey : Point2d units coordinates -> Id
pointToKey =
    Point2d.coordinates
        >> (\( x, y ) ->
                "("
                    ++ quantityToKey x
                    ++ ", "
                    ++ quantityToKey y
                    ++ ")"
           )


quantityToKey : Quantity Float units -> Id
quantityToKey =
    let
        expTolerance =
            6
    in
    Quantity.unwrap >> Round.round expTolerance
