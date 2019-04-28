module SampleData exposing (testGraph)


import Graph



testGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ "p1", "p2", "p3", "p4", "p5", "p6", "q1", "q2", "q3", "q4", "q5", "q6", "r" ]
        [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 6, 7 ), ( 6, 8 ), ( 6, 9 ), ( 6, 10 ), ( 6, 11 ) ]
