module Network exposing
    ( EdgeLabel
    , Entity
    , NodeState
    , Role(..)
    , SimpleGraph
    , Status(..)
    , absoluteEdgeFlow
    , accountList
    , areConnected
    , balanceFromEntity
    , balanceFromNode
    , balanceFromNodeState
    , balanceFromSimpleNode
    , changeAccountBalance
    , changeAccountBalanceOfEntity
    , changeEdgeLabel
    , computeForces
    , connect
    , connectNodeToNodeInList
    , creditNode
    , debitNode
    , defaultNodeState
    , filterNodes
    , getEdgeLabel
    , hiddenTestGraph
    , inComingNodeIds
    , incrementRecruitedCount
    , influencees
    , influencees2
    , influencers
    , initializeNode
    , integerSequence
    , makeTransaction
    , mintCurrency
    , moneySupply
    , netTransactionAmountOfEdgeLabel
    , nodeBalance
    , nodeState
    , nodeStateFromNode
    , outGoingNodeIds
    , postTransactionToContext
    , postTransactionToNetwork
    , randomListElement
    , randomPairs
    , randomTransaction
    , recruitNodes
    , recruitRandom
    , recruitRandomFreeNode
    , reheatGraph
    , removeExpiredCurrencyFromEdges
    , setStatus
    , setupGraph
    , showEdgeLabel
    , simplifyGraph
    , testGraph
    , updateContextWithValue
    , zeroEdgeLabel
    )

import Currency exposing (BankTime, Currency, CurrencyType(..), Expiration(..))
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import IntDict exposing (IntDict)
import List.Extra
import PseudoRandom
import Utility



--
-- TYPES
--


type alias Entity =
    Force.Entity NodeId { value : NodeState }


type alias SimpleGraph =
    Graph NodeState EdgeLabel


type alias EdgeLabel =
    { transactions : List Currency
    }


type alias Network =
    Graph Entity EdgeLabel


type alias NetworkEdge =
    Edge EdgeLabel


type Status
    = Recruited
    | NotRecruited


type Role
    = Shopkeeper
    | Unemployed


type alias GraphId =
    Int


type alias NodeState =
    { name : String
    , status : Status
    , role : Role
    , accountBalance : List Currency
    , parentGraphId : GraphId
    , numberRecruited : Int
    , location : ( Int, Int )
    }



-- TRANSFORM GRAPH --


simplifyGraph : Graph Entity EdgeLabel -> Graph NodeState EdgeLabel
simplifyGraph g =
    Graph.mapNodes (\n -> n.value) g


removeExpiredCurrencyFromEdges : BankTime -> Graph Entity EdgeLabel -> Graph Entity EdgeLabel
removeExpiredCurrencyFromEdges bt g =
    let
        edgeTransformer : EdgeLabel -> EdgeLabel
        edgeTransformer e =
            { e | transactions = Currency.removeInvalid bt e.transactions }
    in
    Graph.mapEdges edgeTransformer g



-- FILTER GRAPH --


filterNodesOnState : (NodeState -> Bool) -> Graph Entity EdgeLabel -> List (Node Entity)
filterNodesOnState filterNodeState graph =
    let
        filterNode : Node Entity -> Bool
        filterNode node =
            node.label |> nodeState |> filterNodeState
    in
    Graph.nodes graph
        |> List.filter filterNode


filterNodes : (Entity -> Bool) -> Graph Entity EdgeLabel -> List (Node Entity)
filterNodes filterNode_ graph =
    let
        filterNode : Node Entity -> Bool
        filterNode node =
            node.label |> filterNode_
    in
    Graph.nodes graph
        |> List.filter filterNode



--
-- TRANSACTIONS: DEBIT, CREDIT, TRANSFER
--


accountList : Graph Entity EdgeLabel -> List ( NodeId, Float )
accountList graph =
    graph
        |> Graph.nodes
        |> List.map (\n -> ( n.id, balanceFromNodeState n.label.value ))


activeTraders : Graph Entity EdgeLabel -> List (Node Entity)
activeTraders graph =
    let
        nodeFilter : Entity -> Bool
        nodeFilter entity =
            balanceFromNodeState (nodeState entity) > 0
    in
    filterNodes nodeFilter graph


debitNode : BankTime -> NodeId -> List Currency -> Graph Entity EdgeLabel -> Graph Entity EdgeLabel
debitNode t nodeId_ incoming graph =
    -- xxx NOTE: improve code
    Graph.mapNodes
        (\n ->
            if n.id == nodeId_ then
                { n
                    | value =
                        { name = n.value.name
                        , status = n.value.status
                        , accountBalance = Currency.debitMany t incoming n.value.accountBalance
                        , parentGraphId = n.value.parentGraphId
                        , role = n.value.role
                        , numberRecruited = n.value.numberRecruited
                        , location = n.value.location
                        }
                }

            else
                n
        )
        graph


creditNode : BankTime -> NodeId -> List Currency -> Graph Entity EdgeLabel -> Graph Entity EdgeLabel
creditNode t nodeId_ incoming graph =
    -- xxx NOTE: improve code!
    Graph.mapNodes
        (\n ->
            if n.id == nodeId_ then
                { n
                    | value =
                        { name = n.value.name
                        , status = n.value.status
                        , accountBalance = Currency.creditMany t incoming n.value.accountBalance
                        , parentGraphId = n.value.parentGraphId
                        , role = n.value.role
                        , numberRecruited = n.value.numberRecruited
                        , location = n.value.location
                        }
                }

            else
                n
        )
        graph


randomTransaction : BankTime -> Maybe Float -> Maybe Float -> Float -> Graph Entity EdgeLabel -> ( Maybe ( NodeId, NodeId ), Graph Entity EdgeLabel )
randomTransaction t mr1 mr2 amount graph =
    let
        traders =
            activeTraders graph
                |> List.map .id

        maybeNodeId1 : Maybe NodeId
        maybeNodeId1 =
            randomListElement mr1 traders

        maybeNodeId2 =
            randomListElement mr2 traders
    in
    if maybeNodeId1 == maybeNodeId2 then
        ( Nothing, graph )

    else
        case ( maybeNodeId1, maybeNodeId2 ) of
            ( Just nodeId1, Just nodeId2 ) ->
                graph
                    |> makeTransaction t nodeId1 nodeId2 amount
                    |> (\g -> ( Just ( nodeId1, nodeId2 ), g ))

            _ ->
                ( Nothing, graph )


makeTransaction : BankTime -> Int -> Int -> Float -> Graph Entity EdgeLabel -> Graph Entity EdgeLabel
makeTransaction currentBankTime i j amount graph =
    case Graph.get i graph of
        Nothing ->
            graph

        Just nodeContext ->
            let
                ( withdrawal, newAccountState ) =
                    Currency.debit currentBankTime amount (accountFromNode nodeContext.node)

                -- xxx
            in
            graph
                |> connectIf i j
                |> creditNode currentBankTime j withdrawal
                |> debitNode currentBankTime i withdrawal
                |> postTransactionToNetwork currentBankTime i j withdrawal
                |> postTransactionToNetwork currentBankTime j i (List.map (scaleTransaction -1) withdrawal)


scaleTransaction : Float -> Currency -> Currency
scaleTransaction multiplier transaction =
    { transaction | amount = multiplier * transaction.amount }


zeroEdgeLabel =
    { transactions = [] }


mintCurrency : CurrencyType -> Float -> Currency
mintCurrency currencyType amount =
    { amount = amount
    , issueTime = 0
    , expiration = Finite 50
    , currencyType = currencyType
    }



-- REPORT: BALANCES, ETC


nodeBalance : NodeId -> Graph Entity EdgeLabel -> Maybe Float
nodeBalance i g =
    g
        |> Graph.nodes
        |> List.filter (\node -> node.id == i)
        |> List.head
        |> Maybe.map (\n -> balanceFromNodeState n.label.value)


balanceFromNodeState : NodeState -> Float
balanceFromNodeState ns =
    ns.accountBalance
        |> List.map .amount
        |> List.sum


balanceFromSimpleNode : Node NodeState -> Float
balanceFromSimpleNode node =
    balanceFromNodeState node.label


balanceFromNode : Node Entity -> Float
balanceFromNode node =
    balanceFromNodeState node.label.value


balanceFromEntity : Entity -> Float
balanceFromEntity ent =
    balanceFromNodeState ent.value


accountFromNode : Node Entity -> List Currency
accountFromNode node =
    node.label.value.accountBalance



-- FLOWS AND OTHER MEASURES ALONG AN EDGE


showEdgeLabel : NodeId -> NodeId -> Graph Entity EdgeLabel -> Maybe EdgeLabel
showEdgeLabel i j g =
    g
        |> Graph.edges
        |> List.filter (\e -> e.from == i && e.to == j)
        |> List.head
        |> Maybe.map (\e -> e.label)


absoluteEdgeFlow : Edge EdgeLabel -> Float
absoluteEdgeFlow e =
    abs (netTransactionAmountOfEdge e)


netTransactionAmountOfEdge : Edge EdgeLabel -> Float
netTransactionAmountOfEdge e =
    netTransactionAmountOfEdgeLabel e.label


netTransactionAmountOfEdgeLabel : EdgeLabel -> Float
netTransactionAmountOfEdgeLabel label =
    label.transactions
        |> List.map .amount
        |> List.sum



-- POST TRANSACTIONS,


postTransactionToNetwork : BankTime -> NodeId -> NodeId -> List Currency -> Graph Entity EdgeLabel -> Graph Entity EdgeLabel
postTransactionToNetwork t n1 n2 transaction g =
    Graph.mapContexts (postTransactionToContext t n1 n2 transaction) g


postTransactionToContext : BankTime -> NodeId -> NodeId -> List Currency -> NodeContext Entity EdgeLabel -> NodeContext Entity EdgeLabel
postTransactionToContext t n1 n2 transaction ctx =
    if ctx.node.id /= n1 then
        ctx

    else
        case ( IntDict.get n2 <| .incoming ctx, IntDict.get n2 <| .outgoing ctx ) of
            ( Just edgeLabel, _ ) ->
                { ctx | incoming = postTransactionToIncoming t n2 transaction ctx.incoming }

            ( _, Just edgeLabel ) ->
                { ctx | outgoing = postTransactionToOutGoing t n2 transaction ctx.outgoing }

            _ ->
                ctx


postTransactionToIncoming : BankTime -> NodeId -> List Currency -> IntDict EdgeLabel -> IntDict EdgeLabel
postTransactionToIncoming t nodeId transaction intDict =
    case IntDict.get nodeId intDict of
        Nothing ->
            intDict

        Just edgeLabel ->
            let
                newEdgeLabel =
                    { edgeLabel | transactions = Currency.creditMany t transaction edgeLabel.transactions }
            in
            IntDict.update nodeId (Maybe.map (\edgeLabel_ -> newEdgeLabel)) intDict


postTransactionToOutGoing : BankTime -> NodeId -> List Currency -> IntDict EdgeLabel -> IntDict EdgeLabel
postTransactionToOutGoing t nodeId transaction intDict =
    case IntDict.get nodeId intDict of
        Nothing ->
            intDict

        Just edgeLabel ->
            let
                newEdgeLabel =
                    { edgeLabel | transactions = Currency.creditMany t transaction edgeLabel.transactions }
            in
            IntDict.update nodeId (Maybe.map (\edgeLabel_ -> newEdgeLabel)) intDict



-- STRING REPRESENTAITON


stringOfEdgeLabel : EdgeLabel -> String
stringOfEdgeLabel el =
    el.transactions
        |> List.map stringFromTransaction
        |> String.join "; "


stringFromTransaction : Currency -> String
stringFromTransaction tr =
    "("
        ++ String.fromFloat (Utility.roundTo 0 tr.amount)
        ++ ", "
        ++ String.fromInt tr.issueTime
        ++ ", "
        ++ stringFromExpiration tr.expiration
        ++ ")"


stringFromExpiration : Expiration -> String
stringFromExpiration exp =
    case exp of
        Infinite ->
            "infinite"

        Finite t ->
            String.fromInt t



-- XXX - MARKERS UP TO HERE


changeEdgeLabel : EdgeLabel -> NodeId -> NodeId -> Edge EdgeLabel -> Edge EdgeLabel
changeEdgeLabel edgeLabel n1_ n2_ e =
    e


getEdgeLabel : NodeId -> NodeId -> Graph Entity EdgeLabel -> Maybe EdgeLabel
getEdgeLabel n1 n2 g =
    case Graph.get n1 g of
        Nothing ->
            Nothing

        Just ctx ->
            case ( IntDict.get n2 ctx.outgoing, IntDict.get n2 ctx.incoming ) of
                ( Just edgeLabel, Nothing ) ->
                    Just edgeLabel

                ( Nothing, Just edgeLabel ) ->
                    Just edgeLabel

                _ ->
                    Nothing


nodeStateFromNode : Node Entity -> NodeState
nodeStateFromNode node =
    node.label.value


nodeState : Entity -> NodeState
nodeState entity =
    entity.value



--
-- INITIALIZATION
--


distributeLocations : Graph Entity EdgeLabel -> Graph Entity EdgeLabel
distributeLocations graph =
    Graph.mapNodes (\node -> node) graph


integerSequence : Int -> Int -> Int -> List Int
integerSequence modulus n seed =
    PseudoRandom.floatSequence n seed ( 0, 1 )
        |> List.map (\x -> round (toFloat modulus * x))
        |> List.Extra.unique
        |> List.filter (\x -> x < modulus)


randomPairs : Int -> Int -> Int -> List ( Int, Int )
randomPairs modulus n seed =
    List.map2 Tuple.pair (integerSequence modulus n seed) (integerSequence modulus n (seed * seed))


setNodeState : GraphId -> Role -> Status -> String -> ( Int, Int ) -> NodeState
setNodeState graphId role status name ( i, j ) =
    { name = name, role = role, status = status, accountBalance = [], parentGraphId = graphId, numberRecruited = 0, location = ( i, j ) }


makeEdge : ( NodeId, NodeId ) -> Edge EdgeLabel
makeEdge ( from, to ) =
    { from = from, to = to, label = zeroEdgeLabel }


testNodes =
    [ Node 0 (setNodeState 0 Shopkeeper NotRecruited "p0" ( 10, 18 ))
    , Node 1 (setNodeState 0 Unemployed NotRecruited "p1" ( 16, 15 ))
    , Node 2 (setNodeState 0 Unemployed NotRecruited "p2" ( 13, 17 ))
    , Node 3 (setNodeState 0 Unemployed NotRecruited "p3" ( 3, 12 ))
    , Node 4 (setNodeState 0 Unemployed NotRecruited "p4" ( 19, 19 ))
    , Node 5 (setNodeState 0 Unemployed NotRecruited "p5" ( 2, 1 ))
    , Node 6 (setNodeState 1 Unemployed NotRecruited "q0" ( 9, 3 ))
    , Node 7 (setNodeState 1 Unemployed NotRecruited "q1" ( 7, 13 ))
    , Node 8 (setNodeState 1 Unemployed NotRecruited "q2" ( 11, 11 ))
    , Node 9 (setNodeState 1 Unemployed NotRecruited "q3" ( 4, 6 ))
    , Node 10 (setNodeState 1 Unemployed NotRecruited "q4" ( 18, 2 ))
    , Node 11 (setNodeState 1 Shopkeeper NotRecruited "q5" ( 17, 8 ))
    , Node 12 (setNodeState 100 Unemployed Recruited "r" ( 8, 10 ))
    ]


hiddenTestGraph =
    Graph.fromNodesAndEdges
        testNodes
        [ makeEdge ( 0, 1 )
        , makeEdge ( 0, 2 )
        , makeEdge ( 0, 3 )
        , makeEdge ( 0, 4 )
        , makeEdge ( 0, 5 )
        , makeEdge ( 6, 7 )
        , makeEdge ( 6, 8 )
        , makeEdge ( 6, 9 )
        , makeEdge ( 6, 10 )
        , makeEdge ( 6, 11 )
        ]


testGraph =
    Graph.fromNodesAndEdges
        testNodes
        []


defaultNodeState =
    { name = "", status = NotRecruited, role = Unemployed, accountBalance = [], parentGraphId = 0, numberRecruited = 0, location = ( 0, 0 ) }


initializeNode : NodeContext NodeState EdgeLabel -> NodeContext Entity EdgeLabel
initializeNode ctx =
    { node = { label = Force.entity ctx.node.id ctx.node.label, id = ctx.node.id }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


setupGraph :
    Graph.Graph NodeState EdgeLabel
    -> ( List (Force.Force Int), Graph.Graph Entity EdgeLabel )
setupGraph inputGraph =
    let
        outputGraph =
            Graph.mapContexts initializeNode inputGraph

        link { from, to } =
            ( from, to )

        forces =
            [ Force.links <| List.map link <| Graph.edges outputGraph
            , Force.manyBody <| List.map .id <| Graph.nodes outputGraph
            , Force.center (500 / 2) (500 / 2)
            ]
    in
    ( forces, outputGraph )


reheatGraph :
    Graph.Graph NodeState EdgeLabel
    -> ( List (Force.Force Int), Graph.Graph Entity EdgeLabel )
reheatGraph inputGraph =
    let
        outputGraph =
            Graph.mapContexts initializeNode inputGraph

        link { from, to } =
            ( from, to )

        forces =
            [ Force.links <| List.map link <| Graph.edges outputGraph
            , Force.manyBody <| List.map .id <| Graph.nodes outputGraph
            , Force.center (500 / 2) (500 / 2)
            ]
    in
    ( forces, outputGraph )



--
-- UPDATE
--


setStatus : Int -> Status -> Graph Entity EdgeLabel -> Graph Entity EdgeLabel
setStatus nodeIndex status graph =
    -- xxx : NOTE improve code
    Graph.mapNodes
        (\n ->
            if n.id == nodeIndex then
                { n
                    | value =
                        { name = n.value.name
                        , status = status
                        , role = n.value.role
                        , accountBalance = n.value.accountBalance
                        , parentGraphId = n.value.parentGraphId
                        , numberRecruited = n.value.numberRecruited
                        , location = n.value.location
                        }
                }

            else
                n
        )
        graph


changeAccountBalance : BankTime -> Int -> List Currency -> Graph Entity EdgeLabel -> Graph Entity EdgeLabel
changeAccountBalance t nodeIndex incoming graph =
    -- xxx NOTE: improve code!
    Graph.mapNodes
        (\n ->
            if n.id == nodeIndex then
                { n
                    | value =
                        { name = n.value.name
                        , status = n.value.status
                        , role = n.value.role
                        , accountBalance = Currency.creditMany t incoming n.value.accountBalance
                        , parentGraphId = n.value.parentGraphId
                        , numberRecruited = n.value.numberRecruited
                        , location = n.value.location
                        }
                }

            else
                n
        )
        graph


changeAccountBalanceOfEntity : BankTime -> List Currency -> Entity -> Entity
changeAccountBalanceOfEntity t incoming entity =
    let
        oldValue =
            entity.value

        newValue =
            { oldValue
                | accountBalance = Currency.creditMany t incoming oldValue.accountBalance
            }
    in
    { entity | value = newValue }


updateContextWithValue : NodeContext Entity EdgeLabel -> Entity -> NodeContext Entity EdgeLabel
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }



--
-- CONNECT
--


connect : NodeId -> NodeId -> Graph n EdgeLabel -> Graph n EdgeLabel
connect from to graph =
    case newContext from to graph of
        Nothing ->
            graph

        Just ctx ->
            Graph.insert ctx graph


connectIf : NodeId -> NodeId -> Graph n EdgeLabel -> Graph n EdgeLabel
connectIf from to graph =
    case newContext from to graph of
        Nothing ->
            graph

        Just ctx ->
            if areConnected from to graph then
                graph

            else
                Graph.insert ctx graph


areConnected : NodeId -> NodeId -> Graph n e -> Bool
areConnected n1 n2 g =
    let
        folder =
            \edge acc ->
                if (edge.to == n1 && edge.from == n2) || (edge.to == n2 && edge.from == n1) then
                    acc || True

                else
                    acc || False
    in
    g |> Graph.edges |> List.foldl folder False


incrementRecruitedCount : NodeId -> Graph Entity EdgeLabel -> Graph Entity EdgeLabel
incrementRecruitedCount nodeId graph =
    -- xxx NOTE: improve code!
    Graph.mapNodes
        (\n ->
            if n.id == nodeId then
                { n
                    | value =
                        { name = n.value.name
                        , status = n.value.status
                        , accountBalance = n.value.accountBalance
                        , role = n.value.role
                        , parentGraphId = n.value.parentGraphId
                        , numberRecruited = n.value.numberRecruited + 1
                        , location = n.value.location
                        }
                }

            else
                n
        )
        graph


connectNodeToNodeInList : NodeId -> List NodeId -> Graph Entity EdgeLabel -> Graph Entity EdgeLabel
connectNodeToNodeInList from nodeList graph =
    List.foldl (\to graph_ -> connect from to graph_ |> setStatus to Recruited) graph nodeList


newContext : NodeId -> NodeId -> Graph n EdgeLabel -> Maybe (NodeContext n EdgeLabel)
newContext from to graph =
    Maybe.map (\x -> { x | outgoing = IntDict.insert to zeroEdgeLabel x.outgoing }) (Graph.get from graph)


newContext2 : NodeId -> NodeId -> Graph n EdgeLabel -> Maybe (NodeContext n EdgeLabel)
newContext2 from to graph =
    Maybe.map (\x -> { x | outgoing = IntDict.insert to zeroEdgeLabel x.outgoing }) (Graph.get from graph)



--
-- CONNECTIONS
--


outGoingNodeIds : NodeId -> Graph n e -> List NodeId
outGoingNodeIds nodeId graph =
    case Graph.get nodeId graph of
        Nothing ->
            []

        Just ctx ->
            ctx.outgoing |> IntDict.keys


inComingNodeIds : NodeId -> Graph n e -> List NodeId
inComingNodeIds nodeId graph =
    case Graph.get nodeId graph of
        Nothing ->
            []

        Just ctx ->
            ctx.incoming |> IntDict.keys



--
-- INFLUENCES
--


{-| If graph contains nodeId -> b, nodeId -> c, etc.,
then influencees a graph returns [b, c, etc]
-}
influencees : NodeId -> Graph n e -> List NodeId
influencees nodeId graph =
    Maybe.map Graph.alongOutgoingEdges (Graph.get nodeId graph)
        |> Maybe.withDefault []


{-| If graph contains a <- b, a <- c, then influencers a graph returns [b,c]
-}
influencers : NodeId -> Graph n e -> List NodeId
influencers nodeId graph =
    Maybe.map Graph.alongIncomingEdges (Graph.get nodeId graph)
        |> Maybe.withDefault []


{-| If graph contains a <- b -> c, then influencers2 a graph returns [c]
-}
influencees2 : NodeId -> Graph n e -> List NodeId
influencees2 nodeId graph =
    List.map (\n -> influencees n graph) (influencers nodeId graph)
        |> List.concat
        |> List.filter (\x -> x /= nodeId)


influencees2b : NodeId -> Graph n e -> List NodeId
influencees2b nodeId graph =
    influencers nodeId graph ++ influencees2 nodeId graph


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


{-| Return the list of nodes of graph that are not in
the nodeExclusionList
-}
nodeComplementOfGraph : Graph n e -> List NodeId -> List (Node n)
nodeComplementOfGraph graph nodeExclusionList =
    Graph.nodes graph
        |> List.Extra.filterNot (\item -> List.member item.id nodeExclusionList)


filterNotGraph : Graph n e -> (Node n -> Bool) -> List (Node n)
filterNotGraph graph filter =
    Graph.nodes graph
        |> List.Extra.filterNot filter



--
-- RECRUITMENT
--


recruitNodes : List Float -> NodeId -> Graph Entity EdgeLabel -> Graph Entity EdgeLabel -> Graph Entity EdgeLabel
recruitNodes rnList recruiterNode currentGraph hiddenGraph_ =
    let
        -- random influencee of recruiterNode
        randomInfluenceeNodeId : Maybe NodeId
        randomInfluenceeNodeId =
            influencees recruiterNode currentGraph
                |> randomListElement (List.Extra.getAt 0 rnList)

        secondOrderInfluencees : List NodeId
        secondOrderInfluencees =
            Maybe.map (\i -> influencees2b i hiddenGraph_) randomInfluenceeNodeId |> Maybe.withDefault []

        randomSecondOrderInfluenceeNodeId : Maybe NodeId
        randomSecondOrderInfluenceeNodeId =
            secondOrderInfluencees
                |> randomListElement (List.Extra.getAt 0 rnList)
    in
    case randomSecondOrderInfluenceeNodeId of
        Nothing ->
            currentGraph

        Just newNodeId ->
            connect recruiterNode newNodeId currentGraph
                |> setStatus newNodeId Recruited


{-| The recruiter recruits a free node at random
-}
recruitRandomFreeNode : List Float -> NodeId -> Graph Entity EdgeLabel -> Graph Entity EdgeLabel
recruitRandomFreeNode numbers recruiter graph =
    let
        freeNodes : List NodeId
        freeNodes =
            nodeComplementOfGraph graph
                (influencees recruiter graph ++ [ recruiter ])
                |> List.map (\n -> n.id)

        rn2 =
            List.Extra.getAt 2 numbers

        freeNode =
            randomListElement rn2 freeNodes
    in
    case freeNode of
        Nothing ->
            graph

        Just nodeId_ ->
            connect recruiter nodeId_ graph
                |> setStatus nodeId_ Recruited
                |> incrementRecruitedCount recruiter


{-| A random influencee whose recruitedCount is 0 or 1
recruits a free node at random.
-}
recruitRandom : List Float -> NodeId -> Graph Entity EdgeLabel -> Graph Entity EdgeLabel
recruitRandom numbers designatedRecruiter graph =
    let
        rn2 =
            List.Extra.getAt 2 numbers

        influenceeNodeIDs =
            influencees designatedRecruiter graph

        influenceeNodes_ =
            filterNodes (\n -> List.member n.id influenceeNodeIDs) graph

        influenceeNodes =
            List.filter (\n -> n.label.value.numberRecruited < 2) influenceeNodes_

        data =
            List.map (\n -> ( n.id, n.label.value.numberRecruited )) influenceeNodes

        recruiter =
            randomListElement rn2 influenceeNodes
                |> Maybe.map (\n -> n.id)

        freeNodes =
            filterNotGraph graph (\node -> node.label.value.status == Recruited)
                |> List.map (\n -> n.id)
                |> List.filter (\id -> id /= 12)

        rn3 =
            List.Extra.getAt 3 numbers

        freeNode =
            randomListElement rn3 freeNodes
    in
    case ( recruiter, freeNode ) of
        ( Just recruiterNodeId, Just recruiteeNodeId ) ->
            let
                freeCurrency =
                    -- xxx
                    { expiration = Finite 300, amount = 10, issueTime = 0, currencyType = Complementary }
            in
            connect recruiterNodeId recruiteeNodeId graph
                |> setStatus recruiteeNodeId Recruited
                |> changeAccountBalance 0 recruiteeNodeId [ freeCurrency ]
                |> incrementRecruitedCount recruiterNodeId

        _ ->
            graph


consIfDefined : Maybe a -> List a -> List a
consIfDefined maybeValue list =
    case maybeValue of
        Nothing ->
            list

        Just element ->
            element :: list



-- FORCES --


computeForces : Graph.Graph n e -> List (Force.Force Int)
computeForces graph =
    let
        k =
            2.0

        link { from, to } =
            ( from, to )
    in
    [ Force.customLinks 1 <| List.map alterLink <| List.map link <| Graph.edges graph
    , Force.manyBodyStrength -1.8 (List.map .id <| Graph.nodes graph)
    , Force.center (500 / 2) (500 / 2)
    ]


alterLink ( from, to ) =
    { source = from
    , target = to
    , distance = 90
    , strength = Just 1
    }


moneySupply : Graph Entity EdgeLabel -> Float
moneySupply graph =
    graph
        |> Graph.nodes
        |> List.map balanceFromNode
        |> List.sum
