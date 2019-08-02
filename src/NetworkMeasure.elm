module NewtworkMeasure exposing (alpha, efficiency, efficiencyOfEdge, resilience, resilienceOfEdge, roundTo, sustainability, sustainabilityPercentage)

import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)



{-
   Functions for the model
-}


efficiencyOfEdge : Float -> Graph -> Edge -> Float
efficiencyOfEdge totalFlow_ network (Edge sourceNode sinkNode flow) =
    let
        edge =
            Edge sourceNode sinkNode flow

        edgeFlow_ =
            edgeFlow edge

        denominator =
            outflowFromNode network sourceNode * inflowToNode network sinkNode

        numerator =
            edgeFlow_ * totalFlow_

        logRatio =
            logBase 2 (numerator / denominator)
    in
    roundTo 3 (edgeFlow_ * logRatio)


resilienceOfEdge : Float -> Graph -> Edge -> Float
resilienceOfEdge totalFlow_ network (Edge sourceNode sinkNode flow) =
    let
        edge =
            Edge sourceNode sinkNode flow

        edgeFlow_ =
            edgeFlow edge

        denominator =
            outflowFromNode network sourceNode * inflowToNode network sinkNode

        numerator =
            edgeFlow_ * edgeFlow_

        logRatio =
            logBase 2 (numerator / denominator)
    in
    edgeFlow_ * logRatio


efficiency : Graph -> Float
efficiency (Graph nodes edges) =
    let
        network =
            Graph nodes edges

        totalFlow_ =
            totalFlow network
    in
    List.map (efficiencyOfEdge totalFlow_ network) edges
        |> List.sum
        |> (\x -> roundTo 3 x)


resilience : Graph -> Float
resilience (Graph nodes edges) =
    let
        network =
            Graph nodes edges

        totalFlow_ =
            totalFlow network
    in
    List.map (resilienceOfEdge totalFlow_ network) edges
        |> List.sum
        |> (\x -> -(roundTo 3 x))


alpha : Graph -> Float
alpha graph =
    let
        ratio =
            1 + (resilience network / efficiency network)
    in
    1 / ratio


sustainability : Graph -> Float
sustainability graph =
    let
        a =
            alpha network

        aa =
            a ^ 1.288

        s =
            -1.844 * aa * logBase 2 aa
    in
    roundTo 4 s


sustainabilityPercentage : Graph -> Float
sustainabilityPercentage network =
    roundTo 2 (100 * sustainability network)



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
