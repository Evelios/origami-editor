module Data.Axioms exposing
    ( Axiom
    , first
    )

{-|


# Huzita-Hatori Axioms

@docs Axiom


# Axiom Actions

@docs first

-}

import Axis2d
import BoundingBox2d exposing (BoundingBox2d)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Util.BoundingBox2d as BoundingBox2d


{-| -}
type Axiom
    = First


{-| Given two distinct points, there is a unique fold that passes through both
of them.
-}
first :
    Point2d units coordinates
    -> Point2d units coordinates
    -> BoundingBox2d units coordinates
    -> LineSegment2d units coordinates
first from to bounded =
    let
        maybeAxis =
            Axis2d.throughPoints from to

        intersections =
            case maybeAxis of
                Just axis ->
                    BoundingBox2d.edges bounded
                        |> List.filterMap (LineSegment2d.intersectionWithAxis axis)

                Nothing ->
                    []
    in
    case intersections of
        pointFrom :: pointTo :: [] ->
            LineSegment2d.from pointFrom pointTo

        _ ->
            LineSegment2d.from from to


{-| Given two points, there is a unique fold that places the first onto the
second.
-}
second :
    Point2d units coordinates
    -> Point2d units coordinates
    -> LineSegment2d units coordinates
second from to =
    LineSegment2d.from from to
