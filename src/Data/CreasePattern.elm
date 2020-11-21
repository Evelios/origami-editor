module Data.CreasePattern exposing
    ( CreasePattern
    , new
    , addVertex, fold, foldBetween
    , edges, vertices
    )

{-|


# Types

@docs CreasePattern


# Builders

@docs new


# Modifiers

@docs addVertex, fold, foldBetween


# Accessors

@docs edges, vertices

-}

import BoundingBox2d exposing (BoundingBox2d)
import Data.Edge exposing (Edge(..))
import Graph exposing (Graph)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)



-- Types


{-| -}
type CreasePattern units coordinates
    = CreasePattern (Graph (Point2d units coordinates) Edge)



-- Builders


{-| -}
new : BoundingBox2d units coordinates -> CreasePattern units coordinates
new boundingBox =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema boundingBox

        { bl, tl, tr, br } =
            { bl = Point2d.xy minX minY
            , tl = Point2d.xy minX maxY
            , tr = Point2d.xy maxX maxY
            , br = Point2d.xy maxX minY
            }

        borders =
            [ LineSegment2d.from bl tl
            , LineSegment2d.from tl tr
            , LineSegment2d.from tr br
            , LineSegment2d.from br bl
            ]
    in
    List.foldl
        (\line -> fold line Boundary)
        (CreasePattern Graph.empty)
        borders



-- Accessors


{-| -}
vertices :
    CreasePattern units coordinates
    -> List (Point2d units coordinates)
vertices (CreasePattern graph) =
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
edges (CreasePattern graph) =
    Graph.edges graph



-- Modifiers


{-| -}
addVertex :
    Point2d units coordinates
    -> CreasePattern units coordinates
    -> CreasePattern units coordinates
addVertex point (CreasePattern graph) =
    Graph.addVertex point graph
        |> CreasePattern


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
foldBetween start end edge (CreasePattern graph) =
    if Graph.hasEdge end start graph then
        CreasePattern graph

    else
        Graph.addEdge start end edge graph
            |> CreasePattern
