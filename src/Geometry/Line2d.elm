module Geometry.Line2d exposing
    ( Line2d
    , through, fromLineSegment
    , directions
    , intersection, bisectors
    , equals
    )

{-|

@docs Line2d

-- Builders

@docs through, fromLineSegment

-- Accessors

@docs directions

-- Intersection

@docs intersection, bisectors

-- Queries

@docs equals

-}

import Angle
import Direction2d exposing (Direction2d)
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


directions : Line2d units coordinates -> ( Direction2d coordinates, Direction2d coordinates )
directions (Line2d _ direction) =
    ( direction, Direction2d.reverse direction )


slope : Line2d units coordinates -> Float
slope (Line2d _ direction) =
    Angle.tan <| Direction2d.toAngle direction


isVertical : Line2d units coordinates -> Bool
isVertical (Line2d _ direction) =
    Direction2d.toAngle direction
        |> Angle.normalize
        |> Angle.inDegrees
        |> (-) 90
        |> abs
        |> (>) Tolerance.float


isHorizontal : Line2d units coordinates -> Bool
isHorizontal (Line2d _ direction) =
    Direction2d.toAngle direction
        |> Angle.normalize
        |> Angle.inDegrees
        |> abs
        |> (>) Tolerance.float


areParallel : Line2d units coordinates -> Line2d units coordinates -> Bool
areParallel line1 line2 =
    (isVertical line1 && isVertical line2)
        || (abs (slope line1 - slope line2) < Tolerance.float)


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



-- Intersection


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



-- Queries


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
                    (getAtY Quantity.zero first)
                    (getAtY Quantity.zero second)

            else
                Point2d.equalWithin
                    Tolerance.quantity
                    (getAtX Quantity.zero first)
                    (getAtX Quantity.zero second)
    in
    directionEquivalence && interceptEquivalence
