module Util.Point2d exposing (..)

import Point2d exposing (Point2d)
import Quantity exposing (Quantity)


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
