module Grid exposing(Cell, CellStatus(..), empty, cellGridFromGraph)

import Network
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import CellGrid exposing(CellGrid)
import Array


-- type alias NodeState = { name: String, status: Status, location: (Int, Int) }

type CellStatus  = Recruited | NotRecruited | Vacant

type alias Cell =
    { status : CellStatus
    , name : String
    }

cellFromEntity : Network.Entity -> Cell
cellFromEntity entity =
   let nodeState_ = Network.nodeState entity
   in
     case nodeState_.status of
       Network.Recruited -> { name = nodeState_.name, status = Recruited}
       Network.NotRecruited -> { name = nodeState_.name, status = NotRecruited}


empty: Int -> Int -> CellGrid Cell
empty rows columns =
    CellGrid.fromList rows columns (List.repeat (rows*columns) {name = "X", status = Vacant})
      |> Maybe.withDefault CellGrid.empty


cellGridFromGraph : Int -> Graph Network.Entity () -> CellGrid Cell
cellGridFromGraph gridWidth graph =
    let
        emptyCellGrid = empty gridWidth gridWidth
     in
        List.foldl insertNode emptyCellGrid (Graph.nodes graph)


insertNode : (Graph.Node Network.Entity) -> CellGrid Cell -> CellGrid Cell
insertNode nodeEntity grid =
    let
      -- (Graph.Node entity) = nodeEntity
      nodeState_ = Network.nodeState nodeEntity.label
    in
    CellGrid.setValue grid nodeState_.location (cellFromEntity nodeEntity.label)




--make : Int -> Graph Network.Entity () -> CellGrid Cell
--make gridWidth graph =
--    let
--        emptyGrid = empty gridWidth gridWidth
--        nodes = Graph.nodes graph
--        List.foldl (\node acc -> ) cellList nodes
--
--     in


foo = 1