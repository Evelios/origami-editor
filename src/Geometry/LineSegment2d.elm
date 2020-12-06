module Geometry.LineSegment2d exposing
    ( axis
    , distanceFrom, within
    , inCartesian, inSvgYDown
    )

{-|


# Accessors

@docs axis


# Queries

@docs distanceFrom, within


# Conversions

@docs inCartesian, inSvgYDown

-}

import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Geometry.Coordinates as Coordinates exposing (Cartesian, SvgYDown)
import Geometry.Tolerance as Tolerance
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Unitless)



-- Accessors


{-| Get the axis that is made from a line segment.
-}
axis : LineSegment2d units coordinates -> Maybe (Axis2d units coordinates)
axis line =
    Axis2d.throughPoints (LineSegment2d.startPoint line) (LineSegment2d.endPoint line)



-- Queries


{-| Get the distance from a line segment to a point.
-}
distanceFrom : Point2d units coordinates -> LineSegment2d units coordinates -> Quantity Float units
distanceFrom point line =
    case axis line of
        Just lineAxis ->
            let
                ( startPoint, endPoint ) =
                    LineSegment2d.endpoints line

                projectedPoint =
                    Point2d.projectOnto lineAxis point

                maybeIntersection =
                    Axis2d.throughPoints point projectedPoint
                        |> Maybe.andThen
                            (\projectedAxis -> LineSegment2d.intersectionWithAxis projectedAxis line)

                pointOnLine =
                    Point2d.distanceFrom point startPoint
                        |> Quantity.plus (Point2d.distanceFrom point endPoint)
                        |> Quantity.equalWithin Tolerance.quantity (LineSegment2d.length line)
            in
            case ( maybeIntersection, pointOnLine ) of
                ( Just closestPoint, _ ) ->
                    Point2d.distanceFrom point closestPoint

                ( _, True ) ->
                    Quantity.zero

                _ ->
                    Quantity.min
                        (Point2d.distanceFrom point startPoint)
                        (Point2d.distanceFrom point endPoint)

        Nothing ->
            Quantity.infinity


{-| Return the line segment that is closest and is within a particular
distance. If there is no such point, return Nothing.
-}
within :
    Quantity Float units
    -> Point2d units coordinates
    -> List (LineSegment2d units coordinates)
    -> Maybe (LineSegment2d units coordinates)
within distance testPoint candidates =
    let
        sortedPoints =
            candidates
                |> List.sortBy (distanceFrom testPoint >> Quantity.unwrap)
    in
    case sortedPoints of
        closest :: _ ->
            if distanceFrom testPoint closest |> Quantity.lessThanOrEqualTo distance then
                Just closest

            else
                Nothing

        _ ->
            Nothing



-- Conversions


{-| -}
inCartesian : BoundingBox2d Pixels SvgYDown -> LineSegment2d Pixels SvgYDown -> LineSegment2d Unitless Cartesian
inCartesian boundingBox =
    let
        { frame, rate } =
            Coordinates.svgYDownToCartesian boundingBox
    in
    LineSegment2d.relativeTo frame >> LineSegment2d.at_ rate


{-| -}
inSvgYDown : BoundingBox2d Pixels SvgYDown -> LineSegment2d Unitless Cartesian -> LineSegment2d Pixels SvgYDown
inSvgYDown boundingBox =
    let
        { frame, rate } =
            Coordinates.svgYDownToCartesian boundingBox
    in
    LineSegment2d.at rate >> LineSegment2d.placeIn frame
