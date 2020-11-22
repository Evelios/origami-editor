module Data.CreasePattern exposing
    ( CreasePattern
    , new
    , addVertex, fold, foldBetween
    , edges, vertices, potentialFolds
    )

{-|


# Types

@docs CreasePattern


# Builders

@docs new


# Modifiers

@docs addVertex, fold, foldBetween


# Accessors

@docs edges, vertices, potentialFolds

-}

import BoundingBox2d exposing (BoundingBox2d)
import Data.Axioms as Axioms
import Data.Edge exposing (Edge(..))
import Graph exposing (Graph)
import LineSegment2d exposing (LineSegment2d)
import List.Extra
import Point2d exposing (Point2d)
import Util.BoundingBox2d as BoundingBox2d



-- Types


{-| -}
type CreasePattern units coordinates
    = CreasePattern (BoundingBox2d units coordinates) (Graph (Point2d units coordinates) Edge)



-- Builders


{-| -}
new : BoundingBox2d units coordinates -> CreasePattern units coordinates
new boundingBox =
    List.foldl
        (\line -> fold line Boundary)
        (CreasePattern boundingBox Graph.empty)
        (BoundingBox2d.edges boundingBox)



-- Accessors


{-| -}
vertices :
    CreasePattern units coordinates
    -> List (Point2d units coordinates)
vertices (CreasePattern _ graph) =
    Graph.vertices graph


{-| -}
edges :
    CreasePattern units coordinates
    ->
        List
            { from : Point2d units coordinates
            , to : Point2d units coordinates
            , data : Edge
            }
edges (CreasePattern _ graph) =
    Graph.edges graph


{-| -}
potentialFolds : CreasePattern units coordinates -> List (LineSegment2d units coordinates)
potentialFolds (CreasePattern boundingBox graph) =
    let
        vertexPairs =
            List.Extra.uniquePairs (Graph.vertices graph)
    in
    List.map (\( from, to ) -> Axioms.first from to boundingBox) vertexPairs



-- Modifiers


{-| -}
addVertex :
    Point2d units coordinates
    -> CreasePattern units coordinates
    -> CreasePattern units coordinates
addVertex point (CreasePattern boundingBox graph) =
    Graph.addVertex point graph
        |> CreasePattern boundingBox


{-| -}
fold :
    LineSegment2d units coordinates
    -> Edge
    -> CreasePattern units coordinates
    -> CreasePattern units coordinates
fold lineSegment =
    let
        ( start, end ) =
            LineSegment2d.endpoints lineSegment
    in
    foldBetween start end


{-| -}
foldBetween :
    Point2d units coordinates
    -> Point2d units coordinates
    -> Edge
    -> CreasePattern units coordinates
    -> CreasePattern units coordinates
foldBetween start end edge (CreasePattern boundingBox graph) =
    if Graph.hasEdge end start graph then
        CreasePattern boundingBox graph

    else
        Graph.addEdge start end edge graph
            |> CreasePattern boundingBox
