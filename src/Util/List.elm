module Util.List exposing (appendIf, cartesianProduct)

{-|

@docs appendIf, cartesianProduct

-}

import List.Extra


{-| -}
appendIf : Maybe a -> List a -> List a
appendIf maybe list =
    case maybe of
        Just a ->
            list ++ [ a ]

        Nothing ->
            list


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct first second =
    first
        |> List.Extra.andThen
            (\x ->
                second
                    |> List.Extra.andThen (\y -> [ ( x, y ) ])
            )
