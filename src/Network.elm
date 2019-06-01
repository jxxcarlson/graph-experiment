module Network
    exposing
        ( Entity
        , Status(..)
        , NodeState
        , defaultNodeState
        , connect
        , setStatus
        , hiddenTestGraph
        , testGraph
        , initializeNode
        , updateContextWithValue
        , outGoingNodeIds
        , inComingNodeIds
        , connectNodeToNodeInList
        , setupGraph
        , computeForces
        , influencees
        , influencers
        , influencees2
        , recruitNodes
        , nodeComplementOfGraph
        , recruitRandomFreeNode
        , recruitRandom
        , randomListElement
        , randomPairs
        , integerSequence
        , nodeState
        )

import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import IntDict
import PseudoRandom
import List.Extra


--
-- TYPES
--


type alias GraphId =
    Int


type alias NodeState =
    { name : String
    , status : Status

    -- , parentGraphId : GraphId
    , numberRecruited : Int
    , location : ( Int, Int )
    }


filterNodes : (NodeState -> Bool) -> Graph Entity () -> List (Node Entity)
filterNodes filterNodeState graph =
    let
        filterNode : Node Entity -> Bool
        filterNode node =
            node.label |> nodeState |> filterNodeState
    in
        Graph.nodes graph
            |> List.filter filterNode


type Status
    = Recruited
    | NotRecruited


type alias Entity =
    Force.Entity NodeId { value : NodeState }


nodeState2 : Node Entity -> NodeState
nodeState2 node =
    node.label.value


nodeState : Entity -> NodeState
nodeState entity =
    entity.value



--
-- INITIALIZATION
--


distributeLocations : Graph Entity () -> Graph Entity ()
distributeLocations graph =
    Graph.mapNodes (\node -> node) graph


integerSequence : Int -> Int -> Int -> List Int
integerSequence modulus n seed =
    PseudoRandom.floatSequence n seed ( 0, 1 )
        |> List.map (\x -> round ((toFloat modulus) * x))
        |> List.Extra.unique
        |> List.filter (\x -> x < modulus)


randomPairs : Int -> Int -> Int -> List ( Int, Int )
randomPairs modulus n seed =
    List.map2 Tuple.pair (integerSequence modulus n seed) (integerSequence modulus n (seed * seed))


unrecruitedNodeState : String -> ( Int, Int ) -> NodeState
unrecruitedNodeState name ( i, j ) =
    { name = name, status = NotRecruited, numberRecruited = 0, location = ( i, j ) }


recruitedNodeState : String -> ( Int, Int ) -> NodeState
recruitedNodeState name ( i, j ) =
    { name = name, status = Recruited, numberRecruited = 0, location = ( i, j ) }


hiddenTestGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ unrecruitedNodeState "p0" ( 10, 18 )
        , unrecruitedNodeState "12" ( 16, 15 )
        , unrecruitedNodeState "p2" ( 13, 17 )
        , unrecruitedNodeState "p3" ( 3, 12 )
        , unrecruitedNodeState "p4" ( 19, 19 )
        , unrecruitedNodeState "p5" ( 2, 1 )
        , unrecruitedNodeState "q0" ( 9, 3 )
        , unrecruitedNodeState "q1" ( 7, 13 )
        , unrecruitedNodeState "q2" ( 11, 11 )
        , unrecruitedNodeState "q3" ( 4, 6 )
        , unrecruitedNodeState "q4" ( 18, 2 )
        , unrecruitedNodeState "q5" ( 17, 8 )
        , recruitedNodeState "r" ( 8, 10 )
        ]
        [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 6, 7 ), ( 6, 8 ), ( 6, 9 ), ( 6, 10 ), ( 6, 11 ) ]


testGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ unrecruitedNodeState "p0" ( 10, 18 )
        , unrecruitedNodeState "12" ( 16, 15 )
        , unrecruitedNodeState "p2" ( 13, 17 )
        , unrecruitedNodeState "p3" ( 3, 12 )
        , unrecruitedNodeState "p4" ( 19, 19 )
        , unrecruitedNodeState "p5" ( 2, 1 )
        , unrecruitedNodeState "q0" ( 9, 3 )
        , unrecruitedNodeState "q1" ( 7, 13 )
        , unrecruitedNodeState "q2" ( 11, 11 )
        , unrecruitedNodeState "q3" ( 4, 6 )
        , unrecruitedNodeState "q4" ( 18, 2 )
        , unrecruitedNodeState "q5" ( 17, 8 )
        , recruitedNodeState "r" ( 8, 10 )
        ]
        []


defaultNodeState =
    { name = "", status = NotRecruited, numberRecruited = 0, location = ( 0, 0 ) }


initializeNode : NodeContext NodeState () -> NodeContext Entity ()
initializeNode ctx =
    { node = { label = Force.entity ctx.node.id ctx.node.label, id = ctx.node.id }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


setupGraph :
    Graph.Graph NodeState ()
    -> ( List (Force.Force Int), Graph.Graph Entity () )
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



--
-- UPDATE
--


setStatus : Int -> Status -> Graph Entity () -> Graph Entity ()
setStatus nodeIndex status graph =
    Graph.mapNodes
        (\n ->
            if n.id == nodeIndex then
                { n
                    | value =
                        { name = n.value.name
                        , status = status
                        , numberRecruited = n.value.numberRecruited
                        , location = n.value.location
                        }
                }
            else
                n
        )
        graph


updateContextWithValue : NodeContext Entity () -> Entity -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
        { nodeCtx | node = { node | label = value } }



--
-- CONNECT
--


connect : NodeId -> NodeId -> Graph n () -> Graph n ()
connect from to graph =
    case newContext from to graph of
        Nothing ->
            graph

        Just ctx ->
            Graph.insert ctx graph


connectNodeToNodeInList : NodeId -> List NodeId -> Graph Entity () -> Graph Entity ()
connectNodeToNodeInList from nodeList graph =
    List.foldl (\to graph_ -> connect from to graph_ |> setStatus to Recruited) graph nodeList


newContext : NodeId -> NodeId -> Graph n () -> Maybe (NodeContext n ())
newContext from to graph =
    Maybe.map (\x -> { x | outgoing = IntDict.insert to () x.outgoing }) (Graph.get from graph)


newContext2 : NodeId -> NodeId -> Graph n () -> Maybe (NodeContext n ())
newContext2 from to graph =
    Maybe.map (\x -> { x | outgoing = IntDict.insert to () x.outgoing }) (Graph.get from graph)



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
    round (x * (toFloat n))


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


recruitNodes : List Float -> NodeId -> Graph Entity () -> Graph Entity () -> Graph Entity ()
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
recruitRandomFreeNode : List Float -> NodeId -> Graph Entity () -> Graph Entity ()
recruitRandomFreeNode numbers recruiter graph =
    let
        freeNodes : List NodeId
        freeNodes =
            nodeComplementOfGraph graph
                ((influencees recruiter graph) ++ [ recruiter ])
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


{-| A random influencee whose recruitedCount is 0 or 1
recruits a free node at random.
-}
recruitRandom : List Float -> NodeId -> Graph Entity () -> Graph Entity ()
recruitRandom numbers designatedRecruiter graph =
    let
        rn2 =
            List.Extra.getAt 2 numbers

        influencees_ =
            influencees designatedRecruiter graph

        -- recruiter =
        --     randomListElement rn2 influencees_
        recruiters =
            filterNodes (\ns -> ns.status == Recruited && ns.numberRecruited < 2) graph

        recruiter =
            (randomListElement rn2 recruiters)
                |> Maybe.map (\n -> n.id)

        freeNodes =
            nodeComplementOfGraph graph
                (designatedRecruiter :: influencees_)
                |> List.map (\n -> n.id)

        -- |>  filterNodes filterNodeState graph
        rn3 =
            List.Extra.getAt 3 numbers

        freeNode =
            randomListElement rn3 freeNodes
    in
        case ( recruiter, freeNode ) of
            ( Just recruiterNodeId, Just recruiteeNodeId ) ->
                connect recruiterNodeId recruiteeNodeId graph
                    |> setStatus recruiteeNodeId Recruited

            _ ->
                graph


consIfDefined : Maybe a -> List a -> List a
consIfDefined maybeValue list =
    case maybeValue of
        Nothing ->
            list

        Just element ->
            element :: list



-- |> setStatus newNodeId Recruited
-- Network.setStatus index Recruited model.graph
--
-- FORCES
--


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
