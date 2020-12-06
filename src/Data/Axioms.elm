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
import Geometry.BoundingBox2d as BoundingBox2d
import Geometry.Line2d as Line2d
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Set.Any as Set
import Util.List
import Util.Tuple as Tuple


{-| -}
type Axiom
    = First
    | Second
    | Third


{-| -}
perform : List Axiom -> CreasePattern units coordinates -> List (LineSegment2d units coordinates)
perform axioms creasePattern =
    let
        creasePatternVertices =
            List.foldl
                Set.insert
                Set.point2d
                (CreasePattern.vertices creasePattern)

        creasePatternEdges =
            List.foldl
                Set.insert
                Set.lineSegment2d
                (CreasePattern.edges creasePattern
                    |> List.map (\{ from, to } -> LineSegment2d.from from to)
                )

        boundingBox =
            CreasePattern.size creasePattern

        addCrease crease generated =
            if Set.member crease creasePatternEdges then
                generated

            else
                Set.insert crease generated

        enumerateAxiom : (a -> b -> BoundingBox2d units coordinates -> c) -> List a -> List b -> List c
        enumerateAxiom axiom la lb =
            List.foldl
                (\( a, b ) lc -> axiom a b boundingBox :: lc)
                []
                (Util.List.cartesianProduct la lb)

        performAxiom axiom generated =
            List.foldl addCrease generated <|
                case axiom of
                    First ->
                        enumerateAxiom first
                            (Set.toList creasePatternVertices)
                            (Set.toList creasePatternVertices)

                    Second ->
                        enumerateAxiom second
                            (Set.toList creasePatternVertices)
                            (Set.toList creasePatternVertices)

                    Third ->
                        enumerateAxiom third
                            (Set.toList creasePatternEdges)
                            (Set.toList creasePatternEdges)
                            |> List.concat
    in
    List.foldl
        performAxiom
        Set.lineSegment2d
        axioms
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
    -> BoundingBox2d units coordinates
    -> LineSegment2d units coordinates
second from to boundingBox =
    let
        connectingLine =
            LineSegment2d.from from to
    in
    case LineSegment2d.perpendicularDirection connectingLine of
        Just perpendicularDirection ->
            let
                perpendicularAxis =
                    Axis2d.through (LineSegment2d.midpoint connectingLine) perpendicularDirection

                intersections =
                    List.filterMap
                        (LineSegment2d.intersectionWithAxis perpendicularAxis)
                        (BoundingBox2d.edges boundingBox)
            in
            case intersections of
                pointFrom :: pointTo :: [] ->
                    LineSegment2d.from pointFrom pointTo

                _ ->
                    connectingLine

        Nothing ->
            connectingLine


third :
    LineSegment2d units coordinates
    -> LineSegment2d units coordinates
    -> BoundingBox2d units coordinates
    -> List (LineSegment2d units coordinates)
third segment1 segment2 boundingBox =
    let
        withinBoundingBox =
            \line -> Line2d.withinBoundingBox line boundingBox
    in
    case ( Line2d.fromLineSegment segment1, Line2d.fromLineSegment segment2 ) of
        ( Just line1, Just line2 ) ->
            case Line2d.bisectors line1 line2 of
                Just bisectors ->
                    -- Folds are not parallel
                    Tuple.mapBoth withinBoundingBox withinBoundingBox bisectors
                        |> Tuple.justToList

                Nothing ->
                    -- Folds are parallel
                    List.filterMap
                        withinBoundingBox
                        [ Line2d.through
                            (Point2d.midpoint (Line2d.yIntercept line1) (Line2d.yIntercept line2))
                            (Line2d.direction line1)
                        ]

        _ ->
            []
