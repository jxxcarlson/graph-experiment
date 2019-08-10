module TestCurrency exposing (acct, c1, c2, c3, m1, m2, suite)

import Currency exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "The Currency module"
        [ describe "Currency.debit"
            -- Nest as many descriptions as you like.
            [ test "debit 1 unit" <|
                \_ ->
                    let
                        result =
                            debit 0 1 acct

                        expected =
                            ( [ { amount = 1, expiration = Infinite, time = 0 } ]
                            , [ { amount = 10, expiration = Infinite, time = 5 }
                              , { amount = 9, expiration = Infinite, time = 0 }
                              ]
                            )
                    in
                    Expect.equal expected result
            , test "credit c1" <|
                \_ ->
                    let
                        result =
                            credit 0 c1 acct

                        expected =
                            [ { amount = 11, expiration = Infinite, time = 0 }
                            , { amount = 10, expiration = Infinite, time = 5 }
                            ]
                    in
                    Expect.equal expected result

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"
            ]
        ]


c1 =
    { amount = 1, time = 0, expiration = Infinite }


c2 =
    { amount = 1, time = 5, expiration = Infinite }


c3 =
    { amount = 1, time = 88, expiration = Infinite }


m1 =
    { amount = 10, time = 0, expiration = Infinite }


m2 =
    { amount = 10, time = 5, expiration = Infinite }


acct =
    [ m1, m2 ]
