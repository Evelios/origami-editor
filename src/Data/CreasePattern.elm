module Data.CreasePattern exposing
    ( CreasePattern
    , new
    , edges, vertices, size
    , fold, foldBetween
    )

{-|


# Types

@docs CreasePattern


# Builders

@docs new


# Accessors

@docs edges, vertices, size


# Modifiers

@docs fold, foldBetween

-}

import BoundingBox2d exposing (BoundingBox2d)
import Data.Edge exposing (Edge(..))
import Geometry.BoundingBox2d as BoundingBox2d
import Graph exposing (Graph)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)



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



-- Private Accessors


graph : CreasePattern units coordinates -> Graph (Point2d units coordinates) Edge
graph (CreasePattern _ theGraph) =
    theGraph



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
vertices =
    graph >> Graph.vertices


{-| -}
edges :
    CreasePattern units coordinates
    ->
        List
            { from : Point2d units coordinates
            , to : Point2d units coordinates
            , data : Edge
            }
edges =
    graph >> Graph.edges



-- Modifiers


{-| -}
fold :
    LineSegment2d units coordinates
    -> Edge
    -> CreasePattern units coordinates
    -> CreasePattern units coordinates
fold line edge (CreasePattern boundingBox theGraph) =
    let
        ( start, end ) =
            LineSegment2d.endpoints line

        addEdge newGraph =
            if Graph.hasEdge end start newGraph then
                theGraph

            else
                Graph.addEdge start end edge newGraph
    in
    List.filterMap
        (\{ from, to } ->
            LineSegment2d.intersectionPoint line
                (LineSegment2d.from from to)
        )
        (Graph.edges theGraph)
        |> List.foldl Graph.addVertex theGraph
        |> addEdge
        |> CreasePattern boundingBox


{-| -}
foldBetween :
    Point2d units coordinates
    -> Point2d units coordinates
    -> Edge
    -> CreasePattern units coordinates
    -> CreasePattern units coordinates
foldBetween from to =
    fold <| LineSegment2d.from from to
