module Utility exposing (integerSequence, randomListElement, randomPairs, roundTo, scale)

import List.Extra
import PseudoRandom



{-

   Auxiliary functions

-}


{-| Create a sequence of integers of length n given a modulus and seed
-}
integerSequence : Int -> Int -> Int -> List Int
integerSequence modulus n seed =
    PseudoRandom.floatSequence n seed ( 0, 1 )
        |> List.map (\x -> round (toFloat modulus * x))
        |> List.Extra.unique
        |> List.filter (\x -> x < modulus)


randomPairs : Int -> Int -> Int -> List ( Int, Int )
randomPairs modulus n seed =
    List.map2 Tuple.pair (integerSequence modulus n seed) (integerSequence modulus n (seed * seed))


scale : Float -> Int -> Int
scale x n =
    round (x * toFloat n)


randomListElement : Maybe Float -> List a -> Maybe a
randomListElement maybeRandomNumber list =
    case maybeRandomNumber of
        Nothing ->
            Nothing

        Just rn ->
            let
                n =
                    List.length list

                i =
                    scale rn (n - 1)
            in
            List.Extra.getAt i list


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
