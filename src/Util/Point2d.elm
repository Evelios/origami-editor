module Util.Point2d exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Data.Coordinates as Coordinates exposing (Cartesian, SvgYDown)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Unitless)


within :
    Quantity Float units
    -> Point2d units coordinates
    -> List (Point2d units coordinates)
    -> Maybe (Point2d units coordinates)
within distance testPoint candidates =
    let
        sortedPoints =
            candidates
                |> List.sortBy (Point2d.distanceFrom testPoint >> Quantity.unwrap)
    in
    case sortedPoints of
        closest :: _ ->
            if Point2d.distanceFrom testPoint closest |> Quantity.lessThanOrEqualTo distance then
                Just closest

            else
                Nothing

        _ ->
            Nothing



-- Conversions


inCartesian : BoundingBox2d Pixels SvgYDown -> Point2d Pixels SvgYDown -> Point2d Unitless Cartesian
inCartesian boundingBox =
    let
        { frame, rate } =
            Coordinates.svgYDownToCartesian boundingBox
    in
    Point2d.relativeTo frame >> Point2d.at_ rate


inSvgYDown : BoundingBox2d Pixels SvgYDown -> Point2d Unitless Cartesian -> Point2d Pixels SvgYDown
inSvgYDown boundingBox =
    let
        { frame, rate } =
            Coordinates.svgYDownToCartesian boundingBox
    in
    Point2d.at rate >> Point2d.placeIn frame
