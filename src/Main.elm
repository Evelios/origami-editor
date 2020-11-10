module Main exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Browser.Dom
import Browser.Events
import Data.Coordinates exposing (Cartesian)
import Element exposing (..)
import Framework.View as View
import Html exposing (Html)
import Pixels exposing (Pixels)
import Point2d
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Init


type alias Model =
    { paperArea : BoundingBox2d Pixels Cartesian
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { paperArea = BoundingBox2d.singleton Point2d.origin
      }
    , Task.attempt ViewAreaResize <|
        Browser.Dom.getViewportOf View.pageId
    )



-- Update


type Msg
    = ViewAreaResize (Result Browser.Dom.Error Browser.Dom.Viewport)
    | BrowserResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserResize _ _ ->
            ( model
            , Task.attempt ViewAreaResize <|
                Browser.Dom.getViewportOf View.pageId
            )

        ViewAreaResize viewportResult ->
            case viewportResult of
                Ok { viewport } ->
                    ( { model
                        | paperArea =
                            BoundingBox2d.withDimensions
                                ( Pixels.float viewport.width
                                , Pixels.float viewport.height
                                )
                                Point2d.origin
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize BrowserResize



-- View


view : Model -> Html msg
view model =
    Element.layout
        [ width fill
        , height fill
        , padding 50
        ]
        (View.page model.paperArea)
