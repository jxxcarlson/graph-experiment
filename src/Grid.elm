module Grid exposing(CellState, CellStatus(..), empty)

import Network
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import CellGrid exposing(CellGrid)


-- type alias NodeState = { name: String, status: Status, location: (Int, Int) }

type CellStatus  = Recruited | NotRecruited | Vacant

type alias CellState =
    { status : CellStatus
    , name : String
    }

cellStateFromNodeState : Network.NodeState -> CellState
cellStateFromNodeState nodeState =
   case nodeState.status of
       Network.Recruited -> { name = nodeState.name, status = Recruited}
       Network.NotRecruited -> { name = nodeState.name, status = NotRecruited}


empty: Int -> Int -> CellGrid CellState
empty rows columns =
    CellGrid.fromList rows columns (List.repeat (rows*columns) {name = "X", status = Vacant})
      |> Maybe.withDefault CellGrid.empty

-- make : Graph Network.Entity () -> CellGrid CellState


foo = 1