module Test.Coordinates exposing (..)

import BoundingBox2d
import Data.Coordinates as Coordinates
import Geometry.Expect
import Pixels
import Point2d
import Test exposing (Test, describe, test)


svgYDownToCartesian : Test
svgYDownToCartesian =
    let
        boundingBox =
            BoundingBox2d.fromExtrema
                { minX = Pixels.pixels 0
                , maxX = Pixels.pixels 100
                , minY = Pixels.pixels 0
                , maxY = Pixels.pixels 100
                }

        { frame, rate } =
            Coordinates.svgYDownToCartesian boundingBox

        testConversion { name, input, expected } =
            test name <|
                \_ ->
                    Point2d.relativeTo frame input
                        |> Point2d.at_ rate
                        |> Geometry.Expect.point2d expected
    in
    describe "svgYDownToCartesian" <|
        List.map testConversion
            [ { name = "At Svg Origin"
              , input = Point2d.pixels 0 0
              , expected = Point2d.unitless -1 1
              }
            , { name = "At Cartesian Origin"
              , input = Point2d.pixels 50 50
              , expected = Point2d.unitless 0 0
              }
            , { name = "At bottom right"
              , input = Point2d.pixels 100 100
              , expected = Point2d.unitless 1 -1
              }
            , { name = "Up left of center"
              , input = Point2d.pixels 60 40
              , expected = Point2d.unitless 0.2 0.2
              }
            , { name = "Down right of center"
              , input = Point2d.pixels 40 60
              , expected = Point2d.unitless -0.2 -0.2
              }
            ]
