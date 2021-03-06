module SampleData exposing (g, gg, newNode, testGraph)

import Graph exposing (..)
import IntDict


testGraph :  Graph String ()
testGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ "p1", "p2", "p3", "p4", "p5", "p6", "q1", "q2", "q3", "q4", "q5", "q6", "r" ]
        [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 6, 7 ), ( 6, 8 ), ( 6, 9 ), ( 6, 10 ), ( 6, 11 ) ]

g :  Graph String ()
g =
    Graph.fromNodeLabelsAndEdgePairs
        [ "A", "B" ]
        []


gg :  Graph String ()
gg =
    Graph.fromNodeLabelsAndEdgePairs
        [ "A", "B" ]
        [ ( 0, 1 ) ]


newNode : { incoming : IntDict.IntDict v
                , node : Graph.Node String
                , outgoing : IntDict.IntDict ()
                }
newNode =
    { node = Node 2 "2"
    , incoming = IntDict.empty
    , outgoing = IntDict.singleton 1 () -- so there will be an edge from 0 to `
    }
