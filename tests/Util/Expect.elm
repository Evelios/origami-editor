module Util.Expect exposing (..)

{-| Utility functions for testing custom data structures.
-}

import Data.AspectRatio as AspectRatio exposing (AspectRatio)
import Expect exposing (Expectation, FloatingPointTolerance(..))


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
