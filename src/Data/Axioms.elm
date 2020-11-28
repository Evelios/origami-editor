module Data.Axioms exposing
    ( Axiom(..)
    , perform
    , first
    )

{-|


# Huzita-Hatori Axioms

@docs Axiom


# Axiom Actions

@docs perform

@docs first

-}

import Axis2d
import BoundingBox2d exposing (BoundingBox2d)
import Data.CreasePattern as CreasePattern exposing (CreasePattern)
import Data.Set as Set exposing (Set)
import LineSegment2d exposing (LineSegment2d)
import List.Extra
import Point2d exposing (Point2d)
import Set.Any as Set
import Util.BoundingBox2d as BoundingBox2d


{-| -}
type Axiom
    = First


{-| This function due to the lack of a reasonable solution runs in exponential time
with respect to the number of the current folds of a crease pattern and the
number of generated folds. This could be optimized in the future if this is
causing performance issues.
-}
perform : Axiom -> CreasePattern units coordinates -> List (LineSegment2d units coordinates)
perform axiom creasePattern =
    let
        vertices =
            List.foldl
                Set.insert
                Set.point2d
                (CreasePattern.vertices creasePattern)

        edges =
            Debug.log "Edges" <|
                List.foldl
                    Set.insert
                    Set.lineSegment2d
                    (CreasePattern.edges creasePattern
                        |> List.map (\{ from, to } -> LineSegment2d.from from to)
                    )

        boundingBox =
            CreasePattern.size creasePattern
    in
    case axiom of
        First ->
            List.foldl
                (\( p1, p2 ) segments ->
                    let
                        crease =
                            first p1 p2 boundingBox
                    in
                    if Set.member crease edges then
                        segments

                    else
                        Set.insert
                            (first p1 p2 boundingBox)
                            segments
                )
                Set.lineSegment2d
                (List.Extra.uniquePairs (Set.toList vertices))
                |> Set.toList


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
