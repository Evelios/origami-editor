module Data.CreasePattern exposing
    ( CreasePattern
    , new
    , edges, vertices, size
    , addVertex, fold, foldBetween
    )

{-|


# Types

@docs CreasePattern


# Builders

@docs new


# Accessors

@docs edges, vertices, size


# Modifiers

@docs addVertex, fold, foldBetween

-}

import BoundingBox2d exposing (BoundingBox2d)
import Data.Edge exposing (Edge(..))
import Graph exposing (Graph)
import LineSegment2d exposing (LineSegment2d)
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


{-| Get the size of the page.
-}
size : CreasePattern units coordinates -> BoundingBox2d units coordinates
size (CreasePattern boundingBox _) =
    boundingBox


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
