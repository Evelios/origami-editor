module Test.BoundingBox2d exposing (..)

import BoundingBox2d
import Geometry.AspectRatio as AspectRatio
import Geometry.BoundingBox2d as BoundingBox2d
import Geometry.Expect
import Pixels
import Point2d
import Test exposing (Test, describe, test)
import Util.Expect as Expect


aspectRatioTest : Test
aspectRatioTest =
    describe "Aspect ratio"
        [ test "from portrait size" <|
            \_ ->
                let
                    boundingBox =
                        BoundingBox2d.withDimensions
                            ( Pixels.pixels 100
                            , Pixels.pixels 150
                            )
                            Point2d.origin

                    actual =
                        AspectRatio.unsafe 1 1.5
                in
                BoundingBox2d.aspectRatio boundingBox
                    |> Expect.aspectRatio actual
        , test "from landscape size" <|
            \_ ->
                let
                    size =
                        BoundingBox2d.withDimensions
                            ( Pixels.pixels 150
                            , Pixels.pixels 100
                            )
                            Point2d.origin

                    expected =
                        AspectRatio.unsafe 1.5 1
                in
                BoundingBox2d.aspectRatio size
                    |> Expect.aspectRatio expected
        ]


shrinkToAspectRatio : Test
shrinkToAspectRatio =
    let
        tests =
            [ { title = "Portrait from square"
              , size = ( Pixels.pixels 200, Pixels.pixels 200 )
              , aspectRatio = AspectRatio.unsafe 1 2
              , expected = ( Pixels.pixels 100, Pixels.pixels 200 )
              }
            , { title = "Landscape from square"
              , size = ( Pixels.pixels 200, Pixels.pixels 200 )
              , aspectRatio = AspectRatio.unsafe 2 1
              , expected = ( Pixels.pixels 200, Pixels.pixels 100 )
              }
            , { title = "Landscape to portrait"
              , size = ( Pixels.pixels 200, Pixels.pixels 100 )
              , aspectRatio = AspectRatio.unsafe 1 2
              , expected = ( Pixels.pixels 50, Pixels.pixels 100 )
              }
            , { title = "Portrait to landscape"
              , size = ( Pixels.pixels 100, Pixels.pixels 200 )
              , aspectRatio = AspectRatio.unsafe 2 1
              , expected = ( Pixels.pixels 100, Pixels.pixels 50 )
              }
            , { title = "Non-normalized aspect ratio portrait to landscape"
              , size = ( Pixels.pixels 300, Pixels.pixels 400 )
              , aspectRatio = AspectRatio.unsafe 3 2
              , expected = ( Pixels.pixels 300, Pixels.pixels 200 )
              }
            , { title = "Non-normalized aspect ratio landscape to portrait"
              , size = ( Pixels.pixels 400, Pixels.pixels 300 )
              , aspectRatio = AspectRatio.unsafe 2 3
              , expected = ( Pixels.pixels 200, Pixels.pixels 300 )
              }
            ]

        testShrink { title, size, aspectRatio, expected } =
            test title <|
                \_ ->
                    BoundingBox2d.withDimensions size Point2d.origin
                        |> BoundingBox2d.shrinkToAspectRatio aspectRatio
                        |> Geometry.Expect.boundingBox2d
                            (BoundingBox2d.withDimensions expected Point2d.origin)
    in
    describe "shrinkToAspectRatio" <| List.map testShrink tests
