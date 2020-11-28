module Data.Hash exposing (..)

{-|


# Builders

@docs fromInt, fromFloat, fromString

-}

import Integer exposing (Integer)
import Murmur3
import Round


type Hash
    = Hash Integer


expTolerance : Int
expTolerance =
    6



-- Builders


{-| -}
fromInt : Int -> Hash
fromInt =
    Integer.fromInt >> Hash


{-| -}
fromFloat : Float -> Hash
fromFloat =
    Round.round expTolerance >> fromString


{-| -}
fromString : String -> Hash
fromString =
    Murmur3.hashString expTolerance >> fromInt



-- Accessors


{-| This is the only reliable way of accessing the hash id. This is the
`comparable` type to use for any other elm library.
-}
toString : Hash -> String
toString (Hash integer) =
    Integer.toString integer



-- Combiners


{-| Combine two hash pairs in a unique way. The order of the parameters matters.
-}
dependent : Hash -> Hash -> Hash
dependent (Hash x) (Hash y) =
    -- Cantors Pairing Function
    -- f(x,y) = (x^2 + 3x + 2xy + y + y^2) / 2
    List.foldl Integer.add
        Integer.zero
        [ x |> Integer.mul x
        , x |> Integer.mul (Integer.fromInt 3)
        , x |> Integer.mul y |> Integer.mul (Integer.fromInt 3)
        , y
        , y |> Integer.mul y
        ]
        |> (\from -> Integer.fromInt 2 |> Integer.div from)
        |> Maybe.withDefault (Integer.fromInt 0)
        |> Hash


{-| Combine two hash pairs such that the order does NOT matter.
-}
independent : Hash -> Hash -> Hash
independent hash1 hash2 =
    case ( hash1, hash2 ) of
        ( Hash id1, Hash id2 ) ->
            if id1 |> Integer.lt id2 then
                dependent hash1 hash2

            else
                dependent hash2 hash1
