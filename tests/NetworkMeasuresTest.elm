module NetworkMeasuresTest exposing (suite)

import Currency exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import NetworkMeasure
import Network exposing(Status(..), Role(..), setNodeState, makeEdge, SimpleNetwork)
import Graph exposing ( Node)
import Data exposing(..)

doTest  label expectation computedValue =
    test label <|
            \_ ->
                expectation  computedValue

suite : Test
suite =
    describe "Network Measures" [

        doTest "Total flow"
          (Expect.within (Expect.Absolute 0.01) 173.8)
          (NetworkMeasure.totalFlow simpleNetwork)

        , doTest "Efficiency"
           (Expect.within (Expect.Absolute 0.1) 154.7)
           (NetworkMeasure.efficiency simpleNetwork)


         ]


