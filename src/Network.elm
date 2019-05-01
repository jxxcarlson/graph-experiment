module Network exposing (Entity, Status(..), NodeState, defaultNodeState, connect, setStatus
  , hiddenTestGraph, testGraph, initializeNode, updateContextWithValue, outGoingNodeIds, inComingNodeIds
  , connectNodeToNodeInList, setupGraph, computeForces, influencees, influencers, influencees2)


import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import IntDict
import Maybe.Extra


--
-- TYPES
--

type alias NodeState = { name: String, status: Status }

type Status = Recruited | NotRecruited

type alias Entity =
    Force.Entity NodeId { value : NodeState }

--
-- INITIALIZATION
--


unrecruitedNodeState : String -> NodeState
unrecruitedNodeState name =
    { name = name, status = NotRecruited }

recruitedNodeState : String -> NodeState
recruitedNodeState name =
    { name = name, status = Recruited }



hiddenTestGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ unrecruitedNodeState "p1", unrecruitedNodeState "p2", unrecruitedNodeState "p3", unrecruitedNodeState "p4"
        , unrecruitedNodeState "p5", unrecruitedNodeState "p6", unrecruitedNodeState "q1", unrecruitedNodeState "q2"
        , unrecruitedNodeState "q3", unrecruitedNodeState "q4", unrecruitedNodeState "q5", unrecruitedNodeState "q6"
        , recruitedNodeState "r" ]
        [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 6, 7 ), ( 6, 8 ), ( 6, 9 ), ( 6, 10 ), ( 6, 11 ) ]


testGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ unrecruitedNodeState "p1", unrecruitedNodeState "p2", unrecruitedNodeState "p3", unrecruitedNodeState "p4"
        , unrecruitedNodeState "p5", unrecruitedNodeState "p6", unrecruitedNodeState "q1", unrecruitedNodeState "q2"
        , unrecruitedNodeState "q3", unrecruitedNodeState "q4", unrecruitedNodeState "q5", unrecruitedNodeState "q6"
        , recruitedNodeState "r" ]
        [  ]

defaultNodeState = { name = "", status = NotRecruited }


initializeNode : NodeContext NodeState () -> NodeContext Entity ()
initializeNode ctx =
    { node = { label = Force.entity ctx.node.id ctx.node.label, id = ctx.node.id }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


setupGraph : Graph.Graph NodeState ()
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
       (forces, outputGraph)


--
-- UPDATE
--


setStatus : Int -> Status -> Graph Entity () -> Graph Entity ()
setStatus  nodeIndex status graph =
    Graph.mapNodes (\n -> if n.id == nodeIndex then { n | value = { name = n.value.name, status = status }}  else n) graph


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

connect: NodeId -> NodeId -> Graph Entity () -> Graph Entity ()
connect from to graph =
    case newContext from to graph of
        Nothing -> graph
        Just ctx -> Graph.insert ctx graph

connectNodeToNodeInList : NodeId -> List NodeId -> Graph Entity () -> Graph Entity ()
connectNodeToNodeInList from nodeList graph =
    List.foldl (\to graph_ -> connect from to graph_) graph nodeList



newContext: NodeId -> NodeId -> Graph Entity () -> Maybe (NodeContext Entity ())
newContext from to graph =
    Maybe.map (\x -> { x | outgoing = IntDict.insert to () x.outgoing} ) (Graph.get from graph)



--
-- CONNECTIONS
--

outGoingNodeIds : NodeId -> Graph Entity () -> List NodeId
outGoingNodeIds nodeId graph =
    case Graph.get nodeId graph of
        Nothing -> []
        Just ctx -> ctx.outgoing |> IntDict.keys


inComingNodeIds : NodeId -> Graph Entity () -> List NodeId
inComingNodeIds nodeId graph =
    case Graph.get nodeId graph of
        Nothing -> []
        Just ctx -> ctx.incoming |> IntDict.keys



--
-- INFLUENCES
--


{-| If graph contains a -> b, a -> c, then influencees a graph returns [b,c]
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
--
-- FORCES
--


computeForces : Graph.Graph n e -> List (Force.Force Int)
computeForces graph =
    let
        k = 2.0
        link { from, to } =
                    ( from,  to )
    in
        [  Force.customLinks 1 <| List.map alterLink <| List.map link <| Graph.edges graph
         , Force.manyBodyStrength -1.8 (List.map .id <| Graph.nodes graph)
         ,  Force.center (500 / 2) (500 / 2)
        ]


alterLink (from, to) =
    { source = from
    , target = to
    , distance = 90
    , strength = Just 1

    }