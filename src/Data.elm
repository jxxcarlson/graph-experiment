module Data exposing (..)


import Currency exposing (..)
import NetworkMeasure
import Network exposing(Status(..), Role(..), setNodeState, makeEdge, SimpleNetwork)
import Graph exposing ( Node)


u1 : Node Network.NodeState
u1 = Node 1 (setNodeState 0 Shopkeeper NotRecruited "U1" ( 10, 18 ))

u2 : Node Network.NodeState
u2 = Node 2 (setNodeState 0 Unemployed NotRecruited "U2" ( 16, 15 ))

u3 : Node Network.NodeState
u3 = Node 3 (setNodeState 0 Unemployed NotRecruited "U3" ( 13, 17 ))

u4 : Node Network.NodeState
u4 =  Node 4 (setNodeState 0 Unemployed NotRecruited "U4" ( 3, 12 ))

testNodes =
    [ u1, u2, u3, u4
    ]

simpleNetwork_ : SimpleNetwork
simpleNetwork_ =
    Graph.fromNodesAndEdges
        testNodes
        [ makeEdge ( 1, 2 )
        , makeEdge ( 1, 4 )
        , makeEdge ( 4, 3 )
        , makeEdge ( 2, 3)
        ]


issueCurrency : Float -> Currency
issueCurrency amount =
    { amount = amount
    , currencyType = Complementary
    , issueTime = 0
    , expiration = Infinite}

simpleTransaction : Float -> Transaction
simpleTransaction amount = [issueCurrency amount]


network : Graph.Graph Network.Entity Network.EdgeLabel
network = Graph.mapContexts Network.initializeNode simpleNetwork_
  |> Network.creditNode 0 1 (simpleTransaction 200)
  |> Network.creditNode 0 2 (simpleTransaction 200)
  |> Network.creditNode 0 3 (simpleTransaction 200)
  |> Network.creditNode 0 4 (simpleTransaction 200)
  |> Network.makeTransaction 0 1 2 90.4
  |> Network.makeTransaction 0 1 4 30
  |> Network.makeTransaction 0 4 3 22
  |> Network.makeTransaction 0 2 3 31.4

simpleNetwork : SimpleNetwork
simpleNetwork =  Network.simplifyGraph network


f12 = 90.4
f14 = 30
f23 = 31.4
f43 = 22

ff = f12 + f14 + f23 + f43