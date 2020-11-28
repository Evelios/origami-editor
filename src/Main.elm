module Main exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Browser
import Browser.Dom
import Browser.Events
import Data.Axioms as Axioms exposing (Axiom(..))
import Data.Coordinates as Coordinates exposing (Cartesian, SvgYDown)
import Data.CreasePattern as CreasePattern exposing (CreasePattern)
import Element exposing (..)
import Framework.Origami as Origami
import Framework.Svg
import Html exposing (Html)
import Html.Attributes
import LineSegment2d exposing (LineSegment2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Unitless)
import Svg exposing (Svg)
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
    { viewArea : BoundingBox2d Pixels SvgYDown
    , creasePattern : CreasePattern Unitless Cartesian
    , axiom : Axiom
    , potentialFolds : List (LineSegment2d Unitless Cartesian)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        creasePattern =
            CreasePattern.new <|
                BoundingBox2d.fromExtrema
                    { minX = Quantity.float -1
                    , maxX = Quantity.float 1
                    , minY = Quantity.float -1
                    , maxY = Quantity.float 1
                    }

        axiom =
            First
    in
    ( { viewArea =
            BoundingBox2d.fromExtrema
                { minX = Pixels.float 0
                , maxX = Pixels.float 1
                , minY = Pixels.float 0
                , maxY = Pixels.float 1
                }
      , creasePattern = creasePattern
      , axiom = axiom
      , potentialFolds = Axioms.perform axiom creasePattern
      }
    , Task.attempt ViewAreaResize <|
        Browser.Dom.getViewportOf ids.viewArea
    )


{-| Collection of div ids that need to be tracked for things like resize events.
-}
ids : { viewArea : String }
ids =
    { viewArea = "viewArea"
    }



-- Update


type Msg
    = ViewAreaResize (Result Browser.Dom.Error Browser.Dom.Viewport)
    | BrowserResize Int Int
    | OnClickPage (Point2d Pixels SvgYDown)
    | OnClickCreasePattern (Point2d Unitless Cartesian)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserResize _ _ ->
            ( model
            , Task.attempt ViewAreaResize <|
                Browser.Dom.getViewportOf ids.viewArea
            )

        ViewAreaResize viewportResult ->
            case viewportResult of
                Ok { viewport } ->
                    ( { model
                        | viewArea =
                            BoundingBox2d.fromExtrema
                                { minX = Quantity.zero
                                , maxX = Pixels.pixels viewport.width
                                , minY = Quantity.zero
                                , maxY = Pixels.pixels viewport.height
                                }
                                |> BoundingBox2d.shrinkToAspectRatio
                                    (BoundingBox2d.aspectRatio
                                        (CreasePattern.size model.creasePattern)
                                    )
                                |> BoundingBox2d.withTopLeft Point2d.origin
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        OnClickPage position ->
            let
                { frame, rate } =
                    Coordinates.svgYDownToCartesian model.viewArea
            in
            update
                (OnClickCreasePattern <|
                    Debug.log "Modified" <|
                        (Point2d.relativeTo frame (Debug.log "Position" position) |> Point2d.at_ rate)
                )
                model

        OnClickCreasePattern _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize BrowserResize



-- View


view : Model -> Html Msg
view model =
    layout
        [ width fill
        , height fill
        , padding 50
        ]
        (el
            [ htmlAttribute <| Html.Attributes.id ids.viewArea
            , width fill
            , height fill
            ]
            (paper model)
        )


paper : Model -> Element Msg
paper model =
    let
        potentialFolds =
            List.map lineToSvg model.potentialFolds

        edges =
            CreasePattern.edges model.creasePattern
                |> List.map
                    ((\{ from, to } -> LineSegment2d.from from to)
                        >> lineToSvg
                    )

        svgElements =
            Origami.page
                []
                { onClick = OnClickPage
                , size = model.viewArea
                }
                :: List.map Origami.potentialFold potentialFolds

        { frame, rate } =
            Coordinates.svgYDownToCartesian model.viewArea

        lineToSvg =
            LineSegment2d.at rate >> LineSegment2d.placeIn frame

        pointToSvg =
            Point2d.at rate >> Point2d.placeIn frame
    in
    Svg.svg
        (Framework.Svg.boundingBoxAttributes model.viewArea)
        svgElements
        |> html
