module Main exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Browser.Dom
import Browser.Events
import Data.AspectRatio as AspectRatio exposing (AspectRatio)
import Data.Coordinates exposing (Cartesian, SvgYDown)
import Data.CreasePattern as CreasePattern exposing (CreasePattern)
import Data.Edge exposing (Edge(..))
import Element exposing (..)
import Framework.Page as Page
import Html exposing (Html)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Task
import Util.BoundingBox2d as BoundingBox2d
import Util.Point2d as Point2d


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Init


type Selectable
    = SelectedVertex (Point2d Pixels Cartesian)


type alias Model =
    { paperArea : BoundingBox2d Pixels Cartesian
    , aspectRatio : AspectRatio
    , creasePattern : CreasePattern Pixels Cartesian
    , hoveredVertex : Maybe Selectable
    , selectedVertex : Maybe Selectable
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        aspectRatio =
            AspectRatio.unsafe 1 1

        paperArea =
            BoundingBox2d.fromExtrema
                { minX = Pixels.float -1
                , maxX = Pixels.float 1
                , minY = Pixels.float -1
                , maxY = Pixels.float 1
                }
                |> BoundingBox2d.shrinkToAspectRatio
                    aspectRatio
    in
    ( { paperArea = paperArea
      , aspectRatio = aspectRatio
      , hoveredVertex = Nothing
      , selectedVertex = Nothing
      , creasePattern = CreasePattern.new paperArea
      }
    , Task.attempt ViewAreaResize <|
        Browser.Dom.getViewportOf Page.pageId
    )



-- Update


type Msg
    = ViewAreaResize (Result Browser.Dom.Error Browser.Dom.Viewport)
    | BrowserResize Int Int
    | PaperHovered (Point2d Pixels Cartesian)
    | PaperClicked (Point2d Pixels Cartesian)
    | PaperExited
    | DeselectVertex


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserResize _ _ ->
            ( model
            , Task.attempt ViewAreaResize <|
                Browser.Dom.getViewportOf Page.pageId
            )

        ViewAreaResize viewportResult ->
            case viewportResult of
                Ok { viewport } ->
                    let
                        paperArea =
                            BoundingBox2d.withDimensions
                                ( Pixels.pixels viewport.width
                                , Pixels.pixels viewport.height
                                )
                                Point2d.origin
                                |> BoundingBox2d.shrinkToAspectRatio model.aspectRatio
                    in
                    ( { model
                        | paperArea = paperArea
                        , creasePattern = CreasePattern.new paperArea
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        PaperClicked position ->
            case Point2d.within (Pixels.pixels 20) position (CreasePattern.vertices model.creasePattern) of
                Just newPoint ->
                    case model.selectedVertex of
                        Just (SelectedVertex oldPoint) ->
                            ( { model
                                | creasePattern =
                                    CreasePattern.foldBetween
                                        newPoint
                                        oldPoint
                                        Valley
                                        model.creasePattern
                                , selectedVertex = Nothing
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model | selectedVertex = Just <| SelectedVertex newPoint }
                            , Cmd.none
                            )

                Nothing ->
                    { model
                        | creasePattern = CreasePattern.addVertex position model.creasePattern
                        , hoveredVertex = Just (SelectedVertex position)
                    }
                        |> update DeselectVertex

        PaperHovered position ->
            ( { model
                | hoveredVertex =
                    Point2d.within
                        (Pixels.pixels 10)
                        position
                        (CreasePattern.vertices model.creasePattern)
                        |> Maybe.map SelectedVertex
              }
            , Cmd.none
            )

        PaperExited ->
            ( { model | hoveredVertex = Nothing }
            , Cmd.none
            )

        DeselectVertex ->
            ( { model | selectedVertex = Nothing }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize BrowserResize



-- View


view : Model -> Html Msg
view model =
    let
        hovered =
            case model.hoveredVertex of
                Just (SelectedVertex vertex) ->
                    Just vertex

                _ ->
                    Nothing

        selected =
            case model.selectedVertex of
                Just (SelectedVertex vertex) ->
                    Just vertex

                _ ->
                    Nothing
    in
    Element.layout
        [ width fill
        , height fill
        , padding 50
        ]
        (Page.view
            { creasePattern = model.creasePattern
            , boundingBox =
                BoundingBox2d.shrinkToAspectRatio
                    model.aspectRatio
                    model.paperArea
            , hoveredVertex = hovered
            , selectedVertex = selected
            , onMouseMove = PaperHovered
            , onMouseClick = PaperClicked
            , onMouseLeave = PaperExited
            }
        )
