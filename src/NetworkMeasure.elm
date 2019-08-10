module NetworkMeasure exposing
    ( alpha
    , efficiency
    , efficiencyOfEdge
    , giniIndex
    , resilience
    , resilienceOfEdge
    , sustainability
    , sustainabilityPercentage
    , totalFlow
    )

import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import IntDict exposing (IntDict)
import Network exposing (EdgeLabel, NodeState, SimpleGraph)
import Utility



{-
   Functions for the model
-}


epsilon =
    0.000001


accountBalances : SimpleGraph -> List Float
accountBalances g =
    g
        |> Graph.nodes
        |> List.map (.label >> Network.balanceFromNodeState)



-- giniIndex : SimpleGraph -> Float


giniIndex g =
    let
        balances =
            accountBalances g

        n =
            List.length balances |> toFloat

        meanBalance =
            List.sum balances / n

        absoluteDifferences =
            differences balances |> List.map abs
    in
    if abs meanBalance < epsilon then
        0

    else
        100 * List.sum absoluteDifferences / (n * n * meanBalance)


{-|

    > differences [1,2,3,4]
    [1,2,3,1,2,1]

-}
differences lst =
    let
        postfixList_ =
            postfixList lst

        differencesForPair pair =
            let
                ( x, lst_ ) =
                    pair
            in
            List.map (\item -> item - x) lst_
    in
    List.map differencesForPair postfixList_ |> List.concat



-- postfixList : List a -> List ( a, List a )


postfixList lst =
    List.indexedMap (\k item -> ( item, List.drop (k + 1) lst )) lst
        |> List.take (List.length lst - 1)


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
            edgeFlow ctx.outgoing |> abs


inflowToNode : NodeId -> SimpleGraph -> Float
inflowToNode nodeId g =
    case Graph.get nodeId g of
        Nothing ->
            0

        Just ctx ->
            edgeFlow ctx.incoming |> abs


totalFlow : SimpleGraph -> Float
totalFlow g =
    g
        |> Graph.edges
        |> List.map Network.absoluteEdgeFlow
        |> List.sum


edgeFlow : IntDict EdgeLabel -> Float
edgeFlow intDict =
    IntDict.values intDict
        |> List.map (Network.netTransactionAmountOfEdgeLabel >> abs)
        |> List.sum


efficiencyOfEdge : Float -> SimpleGraph -> Edge EdgeLabel -> Float
efficiencyOfEdge totalFlow_ g edge =
    let
        edgeFlow_ =
            Network.absoluteEdgeFlow edge

        denominator =
            outflowFromNode edge.from g * inflowToNode edge.to g

        numerator =
            edgeFlow_ * totalFlow_
    in
    case abs denominator < epsilon || abs numerator < epsilon of
        True ->
            0

        False ->
            let
                logRatio =
                    logBase 2 (numerator / denominator)
            in
            Utility.roundTo 3 (edgeFlow_ * logRatio)


resilienceOfEdge : Float -> SimpleGraph -> Edge EdgeLabel -> Float
resilienceOfEdge totalFlow_ g edge =
    let
        edgeFlow_ =
            Network.absoluteEdgeFlow edge

        denominator =
            outflowFromNode edge.from g * inflowToNode edge.to g

        numerator =
            edgeFlow_ * edgeFlow_
    in
    case abs denominator < epsilon || abs numerator < epsilon of
        True ->
            0

        False ->
            let
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
        |> (\x -> Utility.roundTo 3 x)


resilience : SimpleGraph -> Float
resilience g =
    let
        totalFlow_ =
            totalFlow g
    in
    List.map (resilienceOfEdge totalFlow_ g) (Graph.edges g)
        |> List.sum
        |> (\x -> -(Utility.roundTo 3 x))


alpha : SimpleGraph -> Float
alpha g =
    let
        eff =
            efficiency g
    in
    if abs eff < epsilon then
        0

    else
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
    in
    if abs aa < epsilon then
        0

    else
        -1.844 * aa * logBase 2 aa


sustainabilityPercentage : SimpleGraph -> Float
sustainabilityPercentage g =
    Utility.roundTo 2 (100 * sustainability g)
