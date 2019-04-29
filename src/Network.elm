module Network exposing (Entity, Status(..), NodeState, defaultNodeState, connect, setStatus
  , hiddenTestGraph, testGraph, initializeNode, updateContextWithValue, outGoingNodeIds, inComingNodeIds
  , connectNodeToNodeInList)


import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import IntDict


type alias NodeState = { name: String, status: Status }

type Status = Recruited | NotRecruited

type alias Entity =
    Force.Entity NodeId { value : NodeState }

defaultNodeState = { name = "", status = NotRecruited }


initializeNode : NodeContext NodeState () -> NodeContext Entity ()
initializeNode ctx =
    { node = { label = Force.entity ctx.node.id ctx.node.label, id = ctx.node.id }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }



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

smallGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ unrecruitedNodeState "p1", unrecruitedNodeState "p2"]
        [  ]
-- setStatusInEntity  { n | name = n.value.name, status = status }
-- { n | value = n.value }

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

-- Maybe.map (\x -> { x | outgoing = I.insert 1 () x.outgoing} ) (G.get 0 g)

newContext: NodeId -> NodeId -> Graph Entity () -> Maybe (NodeContext Entity ())
newContext from to graph =
    Maybe.map (\x -> { x | outgoing = IntDict.insert to () x.outgoing} ) (Graph.get from graph)


connect: NodeId -> NodeId -> Graph Entity () -> Graph Entity ()
connect from to graph =
    case newContext from to graph of
        Nothing -> graph
        Just ctx -> Graph.insert ctx graph

connectNodeToNodeInList : NodeId -> List NodeId -> Graph Entity () -> Graph Entity ()
connectNodeToNodeInList from nodeList graph =
    List.foldl (\to graph_ -> connect from to graph_) graph nodeList


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