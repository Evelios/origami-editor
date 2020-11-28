module Data.Set exposing
    ( Set
    , lineSegment2d, point2d
    )

{-|


# Types

@docs Set


# Sets

@docs lineSegment2d, point2d

-}

import Data.Hash as Hash exposing (Hash)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Set.Any as Set exposing (AnySet)


type alias Set geometry =
    AnySet String geometry


{-| -}
lineSegment2d : Set (LineSegment2d units coordinates)
lineSegment2d =
    Set.empty (lineToKey >> Hash.toString)


{-| -}
point2d : Set (Point2d units coordinates)
point2d =
    Set.empty (pointToKey >> Hash.toString)


{-| To maintain the fact that two lines are equivalent independent of the order
of the points. The hash function must hash the keys in such a way that the
order doesn't affect the key.
-}
lineToKey : LineSegment2d units coordinates -> Hash
lineToKey =
    LineSegment2d.endpoints
        >> (\( p1, p2 ) -> Hash.independent (pointToKey p1) (pointToKey p2))


pointToKey : Point2d units coordinates -> Hash
pointToKey =
    Point2d.coordinates
        >> (\( x, y ) ->
                Hash.dependent
                    (Hash.fromFloat (Quantity.unwrap x))
                    (Hash.fromFloat (Quantity.unwrap y))
           )
