module Util.Expect exposing (..)

{-| Utility functions for testing custom data structures.
-}

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Geometry.AspectRatio as AspectRatio exposing (AspectRatio)


tolerance =
    Absolute 1.0e-12


{-| Compare aspect ratios.
-}
aspectRatio : AspectRatio -> AspectRatio -> Expectation
aspectRatio first =
    Expect.all
        [ Expect.within tolerance (AspectRatio.xNormalizedBelowOne first) << AspectRatio.xNormalizedBelowOne
        , Expect.within tolerance (AspectRatio.yNormalizedBelowOne first) << AspectRatio.yNormalizedBelowOne
        ]
