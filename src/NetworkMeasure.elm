module NetworkMeasure exposing (alpha, efficiency, efficiencyOfEdge, removeEdgesOfWeightZero, resilience, resilienceOfEdge, roundTo, sustainability, sustainabilityPercentage, totalFlow)

import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import IntDict exposing (IntDict)
import Network exposing (EdgeLabel, NodeState, SimpleGraph)



{-
   Functions for the model
-}


removeEdgesOfWeightZero : SimpleGraph -> SimpleGraph
removeEdgesOfWeightZero g =
    Graph.mapContexts (transformOutgoing >> transformIncoming) g


transformOutgoing : NodeContext NodeState EdgeLabel -> NodeContext NodeState EdgeLabel
transformOutgoing ctx =
    if edgeFlow ctx.outgoing == 0 then
        { ctx | outgoing = IntDict.empty }

    else
        ctx


transformIncoming : NodeContext NodeState EdgeLabel -> NodeContext NodeState EdgeLabel
transformIncoming ctx =
    if edgeFlow ctx.incoming == 0 then
        { ctx | outgoing = IntDict.empty }

    else
        ctx


outflowFromNode : NodeId -> SimpleGraph -> Float
outflowFromNode nodeId g =
    case Graph.get nodeId g of
        Nothing ->
            0

        Just ctx ->
            edgeFlow ctx.outgoing


inflowToNode : NodeId -> SimpleGraph -> Float
inflowToNode nodeId g =
    case Graph.get nodeId g of
        Nothing ->
            0

        Just ctx ->
            edgeFlow ctx.incoming


totalFlow : SimpleGraph -> Float
totalFlow g =
    g
        |> Graph.edges
        |> List.map (.label >> .unitsSent >> toFloat)
        |> List.sum


edgeFlow : IntDict EdgeLabel -> Float
edgeFlow intDict =
    IntDict.values intDict
        |> List.map (.unitsSent >> toFloat)
        |> List.sum


fixDenominator : Float -> Float
fixDenominator x =
    if abs x < 0.000001 then
        1

    else
        x


efficiencyOfEdge : Float -> SimpleGraph -> Edge EdgeLabel -> Float
efficiencyOfEdge totalFlow_ g edge =
    let
        edgeFlow_ =
            edge.label.unitsSent |> toFloat

        denominator =
            fixDenominator <| outflowFromNode edge.from g * inflowToNode edge.to g

        numerator =
            edgeFlow_ * totalFlow_

        logRatio =
            logBase 2 (numerator / denominator)
    in
    roundTo 3 (edgeFlow_ * logRatio)


resilienceOfEdge : Float -> SimpleGraph -> Edge EdgeLabel -> Float
resilienceOfEdge totalFlow_ g edge =
    let
        edgeFlow_ =
            edge.label.unitsSent |> toFloat

        denominator =
            fixDenominator <| outflowFromNode edge.from g * inflowToNode edge.to g

        numerator =
            edgeFlow_ * edgeFlow_

        logRatio =
            logBase 2 (numerator / denominator)
    in
    edgeFlow_ * logRatio


efficiency : SimpleGraph -> Float
efficiency g =
    let
        totalFlow_ =
            totalFlow g
    in
    List.map (efficiencyOfEdge totalFlow_ g) (Graph.edges g)
        |> List.sum
        |> (\x -> roundTo 3 x)


resilience : SimpleGraph -> Float
resilience g =
    let
        totalFlow_ =
            totalFlow g
    in
    List.map (resilienceOfEdge totalFlow_ g) (Graph.edges g)
        |> List.sum
        |> (\x -> -(roundTo 3 x))


alpha : SimpleGraph -> Float
alpha g =
    let
        ratio =
            1 + resilience g / efficiency g
    in
    1 / ratio


sustainability : SimpleGraph -> Float
sustainability g =
    let
        a =
            alpha g

        aa =
            a ^ 1.288

        s =
            -1.844 * aa * logBase 2 aa
    in
    roundTo 4 s


sustainabilityPercentage : SimpleGraph -> Float
sustainabilityPercentage g =
    roundTo 2 (100 * sustainability g)



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
