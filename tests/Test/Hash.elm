module Test.Hash exposing (..)

import Data.Hash as Hash
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz2)


dependent : Test
dependent =
    let
        testHashes hashFn in1 in2 =
            let
                id1 =
                    hashFn in1

                id2 =
                    hashFn in2
            in
            if in1 == in2 then
                Hash.dependent id1 id2
                    |> Expect.equal (Hash.dependent id2 id1)

            else
                Hash.dependent id1 id2
                    |> Expect.notEqual (Hash.dependent id2 id1)
    in
    describe "Order Dependent Hashing"
        [ fuzz2 Fuzz.string Fuzz.string "Strings" <|
            \string1 string2 ->
                testHashes Hash.fromString string1 string2
        , fuzz2 Fuzz.int Fuzz.int "Integers" <|
            \int1 int2 ->
                testHashes Hash.fromInt int1 int2
        , fuzz2 Fuzz.float Fuzz.float "Floats" <|
            \float1 float2 ->
                testHashes Hash.fromFloat float1 float2
        ]


independent : Test
independent =
    let
        testHashes hashFn in1 in2 =
            let
                id1 =
                    hashFn in1

                id2 =
                    hashFn in2
            in
            Hash.independent id1 id2
                |> Expect.equal (Hash.independent id2 id1)
    in
    describe "Order Independent Hashing"
        [ fuzz2 Fuzz.string Fuzz.string "Strings" <|
            \string1 string2 ->
                testHashes Hash.fromString string1 string2
        , fuzz2 Fuzz.int Fuzz.int "Integers" <|
            \int1 int2 ->
                testHashes Hash.fromInt int1 int2
        , fuzz2 Fuzz.float Fuzz.float "Floats" <|
            \float1 float2 ->
                testHashes Hash.fromFloat float1 float2
        ]
