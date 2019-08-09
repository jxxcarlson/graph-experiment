module Utility exposing (roundTo)

{-

   Auxiliary functions

-}


roundTo : Int -> Float -> Float
roundTo places quantity =
    let
        factor =
            10 ^ places

        ff =
            toFloat factor

        q2 =
            ff * quantity

        q3 =
            round q2
    in
    toFloat q3 / ff
