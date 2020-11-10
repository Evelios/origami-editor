module Util.BoundingBox2d exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Data.AspectRatio as AspectRatio exposing (AspectRatio)


aspectRatio : BoundingBox2d units coordinates -> AspectRatio
aspectRatio boundingBox =
    case BoundingBox2d.intervals boundingBox of
        ( width, height ) ->
            AspectRatio.from width height
