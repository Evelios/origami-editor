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
import Quantity
import Task
import Util.BoundingBox2d as BoundingBox2d


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
    , aspectRatio : AspectRatio
    , creasePattern : CreasePattern Pixels Cartesian
    , hoveredVertex : Maybe (Point2d Pixels Cartesian)
    , selectedVertex : Maybe (Point2d Pixels Cartesian)
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
    | PaperMouseDown (Point2d Pixels Cartesian)
    | PaperMouseUp (Point2d Pixels Cartesian)
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

        PaperMouseDown position ->
            case pointWithin position model.creasePattern of
                Just newPoint ->
                    case model.selectedVertex of
                        Just oldPoint ->
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

                        Nothing ->
                            ( { model | selectedVertex = Just newPoint }
                            , Cmd.none
                            )

                Nothing ->
                    { model
                        | creasePattern = CreasePattern.addVertex position model.creasePattern
                        , hoveredVertex = Just position
                    }
                        |> update DeselectVertex

        PaperMouseUp position ->
            case pointWithin position model.creasePattern of
                Just point ->
                    ( model
                    , Cmd.none
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        PaperHovered position ->
            ( { model | hoveredVertex = pointWithin position model.creasePattern }
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


pointWithin : Point2d Pixels Cartesian -> CreasePattern Pixels Cartesian -> Maybe (Point2d Pixels Cartesian)
pointWithin position creasePattern =
    let
        hoverDistance =
            Pixels.float 20

        sortedPoints =
            CreasePattern.vertices creasePattern
                |> List.sortBy (Point2d.distanceFrom position >> Pixels.inPixels)
    in
    case sortedPoints of
        closest :: _ ->
            if Point2d.distanceFrom position closest |> Quantity.lessThanOrEqualTo hoverDistance then
                Just closest

            else
                Nothing

        _ ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize BrowserResize



-- View


view : Model -> Html Msg
view model =
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
            , hoveredVertex = model.hoveredVertex
            , selectedVertex = model.selectedVertex
            , onMouseMove = PaperHovered
            , onMouseDown = PaperMouseDown
            , onMouseUp = PaperMouseUp
            , onMouseLeave = PaperExited
            }
        )
