module Test.LineSegment2d exposing (..)

import Geometry.Expect
import Geometry.LineSegment2d as LineSegment2d
import Length
import LineSegment2d
import Point2d
import Quantity
import Test exposing (Test, describe, test)


distanceFrom : Test
distanceFrom =
    let
        testDistanceFrom { name, line, point, expectation } =
            test name <|
                \_ ->
                    LineSegment2d.distanceFrom point line
                        |> Geometry.Expect.quantity expectation
    in
    describe "distanceFrom" <|
        List.map testDistanceFrom
            [ { name = "Projected distance from point"
              , line =
                    LineSegment2d.from
                        (Point2d.meters 1 2)
                        (Point2d.meters 3 2)
              , point =
                    Point2d.meters 2 3
              , expectation =
                    Length.meters 1
              }
            , { name = "Distance from endpoints"
              , line =
                    LineSegment2d.from
                        (Point2d.meters 1 2)
                        (Point2d.meters 3 2)
              , point =
                    Point2d.meters -1 2
              , expectation =
                    Length.meters 2
              }
            , { name = "Line on segment"
              , line =
                    LineSegment2d.from
                        (Point2d.meters 1 2)
                        (Point2d.meters 3 2)
              , point =
                    Point2d.meters 2 2
              , expectation =
                    Length.meters 0
              }
            , { name = "Line on endpoint"
              , line =
                    LineSegment2d.from
                        (Point2d.meters 1 2)
                        (Point2d.meters 3 2)
              , point =
                    Point2d.meters 3 2
              , expectation =
                    Length.meters 0
              }
            , { name = "Impossible line segment"
              , line =
                    LineSegment2d.from
                        (Point2d.meters 1 2)
                        (Point2d.meters 1 2)
              , point =
                    Point2d.meters 2 3
              , expectation =
                    Quantity.infinity
              }
            ]
