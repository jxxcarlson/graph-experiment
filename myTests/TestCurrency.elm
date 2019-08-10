module TestCurrency exposing (acct, c1, c2, c3, m1, m2)

import Currency exposing (..)


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
