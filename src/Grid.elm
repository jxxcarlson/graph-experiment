module Grid exposing (Cell, CellStatus(..), empty, recruitedCount, cellGridFromGraph)

import Network
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import CellGrid exposing (CellGrid(..))
import Array


-- type alias NodeState = { name: String, status: Status, location: (Int, Int) }


type CellStatus
    = Recruited
    | NotRecruited
    | Vacant


type alias Cell =
    { status : CellStatus
    , name : String
    , id : Int
    }


cellFromEntity : Network.Entity -> Cell
cellFromEntity entity =
    let
        nodeState_ =
            Network.nodeState entity
    in
        case nodeState_.status of
            Network.Recruited ->
                { id = entity.id, name = nodeState_.name, status = Recruited }

            Network.NotRecruited ->
                { id = entity.id, name = nodeState_.name, status = NotRecruited }


empty : Int -> Int -> CellGrid Cell
empty rows columns =
    CellGrid.fromList rows columns (List.repeat (rows * columns) { id = -1, name = "", status = Vacant })
        |> Maybe.withDefault CellGrid.empty


recruitedCount : CellGrid Cell -> Int
recruitedCount (CellGrid _ cellArray) =
    Array.foldl
        (\cell acc ->
            if cell.status == Recruited then
                acc + 1
            else
                acc
        )
        0
        cellArray


cellGridFromGraph : Int -> Graph Network.Entity () -> CellGrid Cell
cellGridFromGraph gridWidth graph =
    let
        emptyCellGrid =
            empty gridWidth gridWidth
    in
        List.foldl insertNode emptyCellGrid (Graph.nodes graph)


insertNode : Graph.Node Network.Entity -> CellGrid Cell -> CellGrid Cell
insertNode nodeEntity grid =
    let
        -- (Graph.Node entity) = nodeEntity
        nodeState_ =
            Network.nodeState nodeEntity.label
    in
        CellGrid.setValue grid nodeState_.location (cellFromEntity nodeEntity.label)
