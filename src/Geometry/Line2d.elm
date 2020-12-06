module Geometry.Line2d exposing
    ( Line2d
    , through, fromLineSegment
    , direction, directions, yIntercept, xIntercept
    , intersection, bisectors, withinBoundingBox, equals
    )

{-|

@docs Line2d

-- Builders

@docs through, fromLineSegment

-- Accessors

@docs direction, directions, yIntercept, xIntercept

-- Queries

@docs intersection, bisectors, withinBoundingBox, equals

-}

import Angle
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Geometry.BoundingBox2d as BoundingBox2d
import Geometry.Direction2d as Direction2d
import Geometry.Tolerance as Tolerance
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)


{-| -}
type Line2d units coordinates
    = Line2d (Point2d units coordinates) (Direction2d coordinates)



-- Builders


{-| -}
through : Point2d units coordinates -> Direction2d coordinates -> Line2d units coordinates
through =
    Line2d


{-| -}
fromLineSegment : LineSegment2d units coordinates -> Maybe (Line2d units coordinates)
fromLineSegment lineSegment =
    let
        ( startPoint, endPoint ) =
            LineSegment2d.endpoints lineSegment
    in
    Direction2d.from startPoint endPoint
        |> Maybe.map (through startPoint)



-- Accessors


direction : Line2d units coordinates -> Direction2d coordinates
direction (Line2d _ theDirection) =
    theDirection


directions : Line2d units coordinates -> ( Direction2d coordinates, Direction2d coordinates )
directions (Line2d _ theDirection) =
    ( theDirection, Direction2d.reverse theDirection )


axis : Line2d units coordinates -> Axis2d units coordinates
axis (Line2d point theDirection) =
    Axis2d.through point theDirection


slope : Line2d units coordinates -> Float
slope (Line2d _ theDirection) =
    Angle.tan <| Direction2d.toAngle theDirection


isVertical : Line2d units coordinates -> Bool
isVertical (Line2d _ theDirection) =
    Direction2d.toAngle theDirection
        |> Angle.normalize
        |> Angle.inDegrees
        |> (-) 90
        |> abs
        |> (>) Tolerance.float


isHorizontal : Line2d units coordinates -> Bool
isHorizontal (Line2d _ theDirection) =
    Direction2d.toAngle theDirection
        |> Angle.normalize
        |> Angle.inDegrees
        |> abs
        |> (>) Tolerance.float


areParallel : Line2d units coordinates -> Line2d units coordinates -> Bool
areParallel line1 line2 =
    (isVertical line1 && isVertical line2)
        || (abs (slope line1 - slope line2) < Tolerance.float)


yIntercept : Line2d units coordinates -> Point2d units coordinates
yIntercept =
    getAtX Quantity.zero


xIntercept : Line2d units coordinates -> Point2d units coordinates
xIntercept =
    getAtY Quantity.zero


getAtX : Quantity Float units -> Line2d units coordinates -> Point2d units coordinates
getAtX xLoc line =
    let
        ( x, y ) =
            coordinates line
    in
    xLoc
        |> Quantity.minus x
        |> Quantity.multiplyBy (slope line)
        |> Quantity.plus y
        |> Point2d.xy xLoc


getAtY : Quantity Float units -> Line2d units coordinates -> Point2d units coordinates
getAtY yLoc line =
    let
        ( x, y ) =
            coordinates line
    in
    yLoc
        |> Quantity.minus y
        |> Quantity.divideBy (slope line)
        |> Quantity.plus x
        |> (\xPoint -> Point2d.xy xPoint yLoc)


{-| Only a private accessor
-}
coordinates : Line2d units coordinates -> ( Quantity Float units, Quantity Float units )
coordinates (Line2d point _) =
    Point2d.coordinates point



-- Queries


{-| -}
intersection : Line2d units coordinates -> Line2d units coordinates -> Maybe (Point2d units coordinates)
intersection line1 line2 =
    let
        ( x1, y1 ) =
            coordinates line1

        ( x2, y2 ) =
            coordinates line2
    in
    if areParallel line1 line2 then
        Nothing

    else if isHorizontal line1 then
        Just <| getAtY y1 line2

    else if isHorizontal line2 then
        Just <| getAtY y2 line1

    else if isVertical line1 then
        Just <| getAtX x1 line2

    else if isVertical line2 then
        Just <| getAtX x2 line1

    else
        -- Standard Intersection
        let
            slope1 =
                slope line1

            slope2 =
                slope line2

            xIntersection =
                (((x2 |> Quantity.multiplyBy slope2)
                    |> Quantity.minus (x1 |> Quantity.multiplyBy slope1)
                 )
                    |> Quantity.minus (y2 |> Quantity.minus y1)
                )
                    |> Quantity.divideBy (slope2 - slope1)
        in
        Just <| getAtX xIntersection line1


bisectors :
    Line2d units coordinates
    -> Line2d units coordinates
    -> Maybe ( Line2d units coordinates, Line2d units coordinates )
bisectors first second =
    if areParallel first second then
        Nothing

    else
        let
            bisectorDirections =
                case ( first, second ) of
                    ( Line2d _ firstDirection, Line2d _ secondDirection ) ->
                        Direction2d.directionsBetween firstDirection secondDirection

            maybeIntersectionPoint =
                intersection first second
        in
        Maybe.map
            (\intersectionPoint ->
                Tuple.mapBoth
                    (through intersectionPoint)
                    (through intersectionPoint)
                    bisectorDirections
            )
            maybeIntersectionPoint


withinBoundingBox :
    Line2d units coordinates
    -> BoundingBox2d units coordinates
    -> Maybe (LineSegment2d units coordinates)
withinBoundingBox line boundingBox =
    let
        intersections =
            List.filterMap
                (LineSegment2d.intersectionWithAxis (axis line))
                (BoundingBox2d.edges boundingBox)
    in
    case intersections of
        start :: end :: [] ->
            Just <| LineSegment2d.from start end

        _ ->
            Nothing


equals : Line2d units coordinates -> Line2d units coordinates -> Bool
equals first second =
    let
        ( firstDirectionOne, firstDirectionTwo ) =
            directions first

        ( secondDirection, _ ) =
            directions second

        directionEquivalence =
            Direction2d.equalWithin Tolerance.quantity firstDirectionOne secondDirection
                || Direction2d.equalWithin Tolerance.quantity firstDirectionTwo secondDirection

        interceptEquivalence =
            if isVertical first || isVertical second then
                Point2d.equalWithin
                    Tolerance.quantity
                    (xIntercept first)
                    (xIntercept second)

            else
                Point2d.equalWithin
                    Tolerance.quantity
                    (yIntercept first)
                    (yIntercept second)
    in
    directionEquivalence && interceptEquivalence
