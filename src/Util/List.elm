module Util.List exposing (appendIf)

{-|

@docs appendIf

-}


{-| -}
appendIf : Maybe a -> List a -> List a
appendIf maybe list =
    case maybe of
        Just a ->
            list ++ [ a ]

        Nothing ->
            list
