module Framework.Page exposing (..)

import BoundingBox2d exposing (BoundingBox2d)
import Color
import Element exposing (Element, html)
import Geometry.Svg as Svg
import Pixels exposing (Pixels)
import Quantity
import Svg
import TypedSvg.Attributes as Attributes
import TypedSvg.Core exposing (Attribute)
import TypedSvg.Types exposing (Paint(..))


view : BoundingBox2d Pixels coordinates -> Element msg
view boundingBox =
    let
        background =
            Svg.boundingBox2d
                [ Attributes.fill <| Paint <| Color.rgb255 100 100 100
                ]
                boundingBox
    in
    Svg.svg
        [ viewBox boundingBox ]
        [ background ]
        |> html


viewBox : BoundingBox2d Pixels coordinates -> Attribute msg
viewBox boundingBox =
    let
        { minX, maxX, minY, maxY } =
            BoundingBox2d.extrema boundingBox
    in
    Attributes.viewBox
        (Quantity.unwrap minX)
        (Quantity.unwrap minY)
        (Quantity.unwrap (maxX |> Quantity.minus minX))
        (Quantity.unwrap (maxY |> Quantity.minus minY))
