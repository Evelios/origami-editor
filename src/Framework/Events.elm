module Framework.Events exposing (onClick, onMouseMove)

{-|


# Events

@docs onClick, onMouseMove

-}

import Data.Coordinates exposing (SvgYDown, SvgYUp)
import Json.Decode as Decode exposing (Decoder)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Svg exposing (Attribute)
import Svg.Events as Events



-- Event Handlers


onClick : (Point2d Pixels SvgYDown -> msg) -> Attribute msg
onClick =
    onEvent "click"


onMouseMove : (Point2d Pixels SvgYDown -> msg) -> Attribute msg
onMouseMove =
    onEvent "mousemove"


{-| Information about handling mouse events can be found on the [elm package
website][1] and on the [Mozilla documentation][2].

[1]: https://package.elm-lang.org/packages/elm/svg/latest/Svg-Events#on
[2]: https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent

-}
onEvent : String -> (Point2d Pixels SvgYDown -> msg) -> Attribute msg
onEvent event onTrigger =
    let
        point2dDecoder : Decoder (Point2d Pixels SvgYDown)
        point2dDecoder =
            Decode.map2
                Point2d.pixels
                (Decode.field "offsetX" Decode.float)
                (Decode.field "offsetY" Decode.float)
    in
    Events.on event (Decode.map onTrigger point2dDecoder)
