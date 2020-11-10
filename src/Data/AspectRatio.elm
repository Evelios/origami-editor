module Data.AspectRatio exposing
    ( AspectRatio
    , from
    , x, y
    , xNormalizedBelowOne, xNormalizedAboveOne, yNormalizedBelowOne, yNormalizedAboveOne
    , unsafe
    )

{-|


# Type

@docs AspectRatio


# Builders

@docs from


# Querying

@docs x, y

@docs xNormalizedBelowOne, xNormalizedAboveOne, yNormalizedBelowOne, yNormalizedAboveOne


# Unsafe

@docs unsafe

-}

import Quantity exposing (Quantity)
import Quantity.Interval as Interval exposing (Interval)


{-| -}
type AspectRatio
    = AspectRatio Float Float



-- Constructors


{-| Construct an aspect ratio from the width and height of the object.
-}
from : Interval number coordinates -> Interval number coordinates -> AspectRatio
from width height =
    AspectRatio
        (Quantity.unwrap <| Interval.width width)
        (Quantity.unwrap <| Interval.width height)



-- Accessors


{-| Alias for xNormalizedBelowOne
-}
x : AspectRatio -> Float
x =
    xNormalizedBelowOne


{-| Alias for yNormalizedBelowOne
-}
y : AspectRatio -> Float
y =
    yNormalizedBelowOne


{-| Get the y value of the aspect ratio normalized so that the larger value
is larger than one.

    -- 1 : 1.5
    aspectRatio 2 3
        |> xNormalizedAboveOne
    --> 1

-}
xNormalizedAboveOne : AspectRatio -> Float
xNormalizedAboveOne (AspectRatio xVal yVal) =
    xVal / min xVal yVal


{-| Get the y value of the aspect ratio normalized so that the larger value
is larger than one.

    -- 1 : 1.5
    aspectRatio 2 3
        |> yNormalizedAboveOne

    --> 1.5

-}
yNormalizedAboveOne : AspectRatio -> Float
yNormalizedAboveOne (AspectRatio xVal yVal) =
    yVal / min xVal yVal


{-| Get the x value of the aspect ratio normalized so that the larger value
is equal to one.

    -- 0.66 : 1
    aspectRatio 2 3
        |> xNormalizedBelowOne

    --> 0.66

-}
xNormalizedBelowOne : AspectRatio -> Float
xNormalizedBelowOne (AspectRatio xVal yVal) =
    xVal / max xVal yVal


{-| Get the x value of the aspect ratio normalized so that the larger value
is equal to one.

    -- 0.66 : 1
    aspectRatio 2 3
        |> yNormalizedBelowOne

    --> 1

-}
yNormalizedBelowOne : AspectRatio -> Float
yNormalizedBelowOne (AspectRatio xVal yVal) =
    yVal / max xVal yVal



-- Unsafe


{-| Construct an aspect ratio from the width and height of an object. This
function has no protection on inputs. Only use this function if you know that
both values are greater than zero. This should not be used for user input.
-}
unsafe : Float -> Float -> AspectRatio
unsafe xin yin =
    AspectRatio xin yin
