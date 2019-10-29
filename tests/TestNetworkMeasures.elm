module TestNetworkMeasures exposing (suite)

import Currency exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Newtork Measures" [
             test "credit c1" <|
                \_ ->
                    let
                        result =  1
                        expected = 1

                    in
                    Expect.equal expected result



         ]


