module Test.Line2d exposing (..)

import Direction2d
import Expect
import Extra.Expect as Expect
import Geometry.Expect as Expect
import Geometry.Line2d as Line2d
import Geometry.Tolerance as Tolerance
import Point2d
import Test exposing (Test, describe, test)


intersection : Test
intersection =
    let
        testIntersection { name, firstLine, secondLine, expectation } =
            test name <|
                \_ ->
                    let
                        maybeIntersection =
                            Line2d.intersection firstLine secondLine
                    in
                    case expectation of
                        Just expectedIntersection ->
                            case maybeIntersection of
                                Just pointIntersection ->
                                    pointIntersection
                                        |> Expect.point2dWithin
                                            Tolerance.quantity
                                            expectedIntersection

                                Nothing ->
                                    Expect.fail "There should be a point intersection"

                        Nothing ->
                            case maybeIntersection of
                                Just _ ->
                                    Expect.fail "Expected no point intersection"

                                Nothing ->
                                    Expect.pass

        point1 =
            Point2d.unitless 1 2

        point2 =
            Point2d.unitless -2 1
    in
    describe "Line2d intersection" <|
        List.map testIntersection
            [ { name = "Standard intersection"
              , firstLine = Line2d.through point1 (Direction2d.degrees 45)
              , secondLine = Line2d.through point2 (Direction2d.degrees 135)
              , expectation = Just <| Point2d.unitless -1 0
              }
            , { name = "First line with zero slope"
              , firstLine = Line2d.through point1 (Direction2d.degrees 0)
              , secondLine = Line2d.through point2 (Direction2d.degrees 135)
              , expectation = Just <| Point2d.unitless -3 2
              }
            , { name = "Second line with zero slope"
              , firstLine = Line2d.through point1 (Direction2d.degrees 45)
              , secondLine = Line2d.through point2 (Direction2d.degrees 0)
              , expectation = Just <| Point2d.unitless 0 1
              }
            , { name = "First line with infinite slope"
              , firstLine = Line2d.through point1 (Direction2d.degrees 90)
              , secondLine = Line2d.through point2 (Direction2d.degrees 135)
              , expectation = Just <| Point2d.unitless 1 -2
              }
            , { name = "Second line with infinite slope"
              , firstLine = Line2d.through point1 (Direction2d.degrees 45)
              , secondLine = Line2d.through point2 (Direction2d.degrees 90)
              , expectation = Just <| Point2d.unitless -2 -1
              }
            , { name = "Parallel Lines"
              , firstLine = Line2d.through point1 (Direction2d.degrees 45)
              , secondLine = Line2d.through point2 (Direction2d.degrees 45)
              , expectation = Nothing
              }
            ]


bisectors : Test
bisectors =
    let
        testBisectors { name, firstLine, secondLine, expectation } =
            test name <|
                \_ ->
                    case ( expectation, Line2d.bisectors firstLine secondLine ) of
                        ( Nothing, Nothing ) ->
                            Expect.pass

                        ( Just ( firstExpectation, secondExpectation ), Just ( firstBisector, secondBisector ) ) ->
                            if
                                (Line2d.equals firstExpectation firstBisector
                                    && Line2d.equals secondExpectation secondBisector
                                )
                                    || (Line2d.equals firstExpectation secondBisector
                                            && Line2d.equals secondExpectation firstBisector
                                       )
                            then
                                Expect.pass

                            else
                                Expect.equal expectation (Just ( firstBisector, secondBisector ))

                        ( _, results ) ->
                            Expect.equal expectation results
    in
    describe "Finding line bisectors" <|
        List.map testBisectors
            [ { name = "Parallel Lines"
              , firstLine = Line2d.through Point2d.origin (Direction2d.degrees 30)
              , secondLine = Line2d.through (Point2d.unitless 1 1) (Direction2d.degrees 30)
              , expectation = Nothing
              }
            , { name = "Overlapping lines"
              , firstLine = Line2d.through Point2d.origin (Direction2d.degrees 30)
              , secondLine = Line2d.through Point2d.origin (Direction2d.degrees 30)
              , expectation = Nothing
              }
            , { name = "Cartesian Lines"
              , firstLine = Line2d.through Point2d.origin (Direction2d.degrees 90)
              , secondLine = Line2d.through Point2d.origin (Direction2d.degrees 180)
              , expectation =
                    Just
                        ( Line2d.through Point2d.origin (Direction2d.degrees 45)
                        , Line2d.through Point2d.origin (Direction2d.degrees 135)
                        )
              }
            , { name = "Rotated 45 Cartesian Lines"
              , firstLine = Line2d.through Point2d.origin (Direction2d.degrees 45)
              , secondLine = Line2d.through Point2d.origin (Direction2d.degrees 135)
              , expectation =
                    Just
                        ( Line2d.through Point2d.origin (Direction2d.degrees 90)
                        , Line2d.through Point2d.origin (Direction2d.degrees 180)
                        )
              }
            , { name = "Offset and Rotated 45 Cartesian Lines"
              , firstLine = Line2d.through Point2d.origin (Direction2d.degrees 45)
              , secondLine = Line2d.through (Point2d.unitless 1 1) (Direction2d.degrees 135)
              , expectation =
                    Just
                        ( Line2d.through (Point2d.unitless 1 1) (Direction2d.degrees 90)
                        , Line2d.through (Point2d.unitless 1 1) (Direction2d.degrees 180)
                        )
              }
            ]
