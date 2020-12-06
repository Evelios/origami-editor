module Util.Tuple exposing (justToList)

{-|


# Helper Functions

@docs justToList

-}


{-| -}
justToList : ( Maybe a, Maybe a ) -> List a
justToList tuple =
    case tuple of
        ( Just a, Just b ) ->
            [ a, b ]

        ( Just a, Nothing ) ->
            [ a ]

        ( Nothing, Just b ) ->
            [ b ]

        ( Nothing, Nothing ) ->
            []
