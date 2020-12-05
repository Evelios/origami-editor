module Extra.Expect exposing (..)

import Direction2d
import Expect exposing (Expectation)
import Geometry.Line2d as Line2d exposing (Line2d)
import Geometry.Tolerance as Tolerance


line2d : Line2d units coordinates -> Line2d units coordinates -> Expectation
line2d first second =
    let
        ( firstDirectionOne, firstDirectionTwo ) =
            Line2d.directions first

        ( secondDirectionOne, _ ) =
            Line2d.directions second
    in
    if
        Direction2d.equalWithin Tolerance.quantity firstDirectionOne secondDirectionOne
            || Direction2d.equalWithin Tolerance.quantity firstDirectionTwo secondDirectionOne
    then
        Expect.pass

    else
        Expect.fail "Lines are not equal"
