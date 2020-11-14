module Main exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Browser.Dom
import Browser.Events
import Data.AspectRatio as AspectRatio exposing (AspectRatio)
import Data.Coordinates exposing (Cartesian, SvgYDown)
import Data.Edge exposing (Edge)
import Element exposing (..)
import Framework.Page as Page
import Graph exposing (Graph)
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


type alias PointGraph =
    Graph (Point2d Pixels Cartesian) Edge


type alias Model =
    { paperArea : BoundingBox2d Pixels Cartesian
    , aspectRatio : AspectRatio
    , graph : PointGraph
    , hoveredVertex : Maybe (Point2d Pixels Cartesian)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { paperArea =
            BoundingBox2d.fromExtrema
                { minX = Pixels.float -1
                , maxX = Pixels.float 1
                , minY = Pixels.float -1
                , maxY = Pixels.float 1
                }
      , aspectRatio = AspectRatio.unsafe 1 1
      , hoveredVertex = Nothing
      , graph = Graph.empty
      }
    , Task.attempt ViewAreaResize <|
        Browser.Dom.getViewportOf Page.pageId
    )



-- Update


type Msg
    = ViewAreaResize (Result Browser.Dom.Error Browser.Dom.Viewport)
    | BrowserResize Int Int
    | PaperClicked (Point2d Pixels Cartesian)
    | PaperHovered (Point2d Pixels Cartesian)
    | PaperExited


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
                    ( { model
                        | paperArea =
                            BoundingBox2d.withDimensions
                                ( Pixels.pixels viewport.width
                                , Pixels.pixels viewport.height
                                )
                                Point2d.origin
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        PaperClicked position ->
            ( { model
                | graph = Graph.addVertex position model.graph
                , hoveredVertex = Just position
              }
            , Cmd.none
            )

        PaperHovered position ->
            ( { model | hoveredVertex = pointHovered position model.graph }
            , Cmd.none
            )

        PaperExited ->
            ( { model | hoveredVertex = Nothing }
            , Cmd.none
            )


pointHovered : Point2d Pixels Cartesian -> PointGraph -> Maybe (Point2d Pixels Cartesian)
pointHovered position graph =
    let
        hoverDistance =
            Pixels.float 20

        sortedPoints =
            Graph.vertices graph
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
            { graph = model.graph
            , boundingBox =
                BoundingBox2d.shrinkToAspectRatio
                    model.aspectRatio
                    model.paperArea
            , hoveredVertex = model.hoveredVertex
            , onClick = PaperClicked
            , onMouseMove = PaperHovered
            , onMouseLeave = PaperExited
            }
        )
