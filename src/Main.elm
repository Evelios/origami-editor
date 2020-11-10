module Main exposing (..)

import Browser
import Element exposing (none)
import Html exposing (Html)
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
-- Init
type alias Model = {}


init : () -> (Model, Cmd Msg)
init _ =
    ({}, Cmd.none)


-- Update


type Msg = None

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View

view : Model -> Html msg
view _ =
    Element.layout [] none