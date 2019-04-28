module Main exposing (main, initializeNode, linkElement, nodeElement)

{-| This demonstrates laying out the characters in Les Miserables
based on their co-occurence in a scene. Try dragging the nodes!
-}

import Browser
import Browser.Events
import Color
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Border as Border
import Html exposing(Html)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import Time
import TypedSvg exposing (circle, g, line, svg, title, text_)
import TypedSvg.Attributes exposing (class, fill, stroke, viewBox, fontSize, transform)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core as Svg exposing (Attribute, Svg)
import TypedSvg.Types exposing (Fill(..), Length(..), Transform(..))



unrecruitedNodeState : String -> NodeState
unrecruitedNodeState name =
    { name = name, status = NotRecruited }

recruitedNodeState : String -> NodeState
recruitedNodeState name =
    { name = name, status = Recruited }

testGraph2 =
    Graph.fromNodeLabelsAndEdgePairs
        [ unrecruitedNodeState "p1", unrecruitedNodeState "p2", unrecruitedNodeState "p3", unrecruitedNodeState "p4"
        , unrecruitedNodeState "p5", unrecruitedNodeState "p6", unrecruitedNodeState "q1", unrecruitedNodeState "q2"
        , unrecruitedNodeState "q3", unrecruitedNodeState "q4", unrecruitedNodeState "q5", unrecruitedNodeState "q6", unrecruitedNodeState "r" ]
        [ ( 0, 1 ), ( 0, 2 ), ( 0, 3 ), ( 0, 4 ), ( 0, 5 ), ( 6, 7 ), ( 6, 8 ), ( 6, 9 ), ( 6, 10 ), ( 6, 11 ) ]


testGraph =
    Graph.fromNodeLabelsAndEdgePairs
        [ unrecruitedNodeState "p1", unrecruitedNodeState "p2", unrecruitedNodeState "p3", unrecruitedNodeState "p4"
        , unrecruitedNodeState "p5", unrecruitedNodeState "p6", unrecruitedNodeState "q1", unrecruitedNodeState "q2"
        , unrecruitedNodeState "q3", unrecruitedNodeState "q4", unrecruitedNodeState "q5", unrecruitedNodeState "q6"
        , recruitedNodeState "r" ]
        [  ]



main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }

type Msg
    = DragStart NodeId ( Float, Float )
    | DragAt ( Float, Float )
    | DragEnd ( Float, Float )
    | Tick Time.Posix


type alias Model =
    { drag : Maybe Drag
    , graph : Graph Entity ()
    , simulation : Force.State NodeId
    }


type alias Drag =
    { start : ( Float, Float )
    , current : ( Float, Float )
    , index : NodeId
    }

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


init : () -> ( Model, Cmd Msg )
init _ =
    let
        graph =
            Graph.mapContexts initializeNode testGraph

        link { from, to } =
            ( from, to )

        forces =
            [ Force.links <| List.map link <| Graph.edges graph
            , Force.manyBody <| List.map .id <| Graph.nodes graph
            , Force.center (500 / 2) (500 / 2)
            ]
    in
        ( Model Nothing graph (Force.simulation forces), Cmd.none )


updateNode : ( Float, Float ) -> NodeContext Entity () -> NodeContext Entity ()
updateNode ( x, y ) nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
        updateContextWithValue nodeCtx { nodeValue | x = x, y = y }


updateContextWithValue : NodeContext Entity () -> Entity -> NodeContext Entity ()
updateContextWithValue nodeCtx value =
    let
        node =
            nodeCtx.node
    in
        { nodeCtx | node = { node | label = value } }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
        List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


update : Msg -> Model -> Model
update msg ({ drag, graph, simulation } as model) =
    case msg of
        Tick t ->
            let
                ( newState, list ) =
                    Force.tick simulation <| List.map .label <| Graph.nodes graph
            in
                case drag of
                    Nothing ->
                        Model drag (updateGraphWithList graph list) newState

                    Just { current, index } ->
                        Model drag
                            (Graph.update index
                                (Maybe.map (updateNode current))
                                (updateGraphWithList graph list)
                            )
                            newState

        DragStart index xy ->
            Model (Just (Drag xy xy index)) graph simulation

        DragAt xy ->
            case drag of
                Just { start, index } ->
                    Model (Just (Drag start xy index))
                        (Graph.update index (Maybe.map (updateNode xy)) graph)
                        (Force.reheat simulation)

                Nothing ->
                    Model Nothing graph simulation

        DragEnd xy ->
            case drag of
                Just { start, index } ->
                    Model Nothing
                        (Graph.update index (Maybe.map (updateNode xy)) graph)
                        simulation

                Nothing ->
                    Model Nothing graph simulation


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            -- This allows us to save resources, as if the simulation is done, there is no point in subscribing
            -- to the rAF.
            if Force.isCompleted model.simulation then
                Sub.none
            else
                Browser.Events.onAnimationFrame Tick

        Just _ ->
            Sub.batch
                [ Browser.Events.onMouseMove (Decode.map (.clientPos >> DragAt) Mouse.eventDecoder)
                , Browser.Events.onMouseUp (Decode.map (.clientPos >> DragEnd) Mouse.eventDecoder)
                , Browser.Events.onAnimationFrame Tick
                ]


onMouseDown : NodeId -> Attribute Msg
onMouseDown index =
    Mouse.onDown (.clientPos >> DragStart index)

linkElement : Graph (Force.Entity Int { value : NodeState }) e
                    -> { a | from : Graph.NodeId, to : Graph.NodeId }
                    -> Svg msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 defaultNodeState) <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 defaultNodeState) <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
        line
            [ strokeWidth 1
            , stroke (Color.rgb255 170 170 170)
            , x1 source.x
            , y1 source.y
            , x2 target.x
            , y2 target.y
            ]
            []


nodeElement : Node { a | value : NodeState, x : Float, y : Float } -> Svg Msg
nodeElement node =
    g []
        [ circle
            [ r 14.0
            , if node.label.value.status == NotRecruited then  fill (Fill (Color.rgba 0.3 0 1 1.0)) else  fill (Fill (Color.rgba 1.0 0 0.3 1.0))
            , stroke (Color.rgba 0 0 0 0)
            , strokeWidth 7
            , onMouseDown node.id
            , cx node.label.x
            , cy node.label.y
            ]
            [ title [] [ Svg.text node.label.value.name ] ]
        , text_
            [ transform [ Translate (node.label.x - 6) (node.label.y + 3) ]
            , (fontSize (Px 12))
            , stroke (Color.rgba 1 1 1 1)
            ]
            [ Svg.text node.label.value.name ]
        ]

--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    row [ spacing 24, centerX, centerY ] [
         leftPanel model
       , rightPanel model

      ]

leftPanel : Model -> Element Msg
leftPanel model =
    column [spacing 12, width (px 500)] [
       el [] (text "GRAPH")
    ]

rightPanel : Model -> Element Msg
rightPanel model =
    column [spacing 12, width (px 500), Border.width 1] [
      viewGraph model  500 500 |> Element.html
    ]

viewGraph : Model -> Float -> Float -> Html Msg
viewGraph model w h =
    svg [ viewBox 0 0 w h ]
        [ Graph.edges model.graph
            |> List.map (linkElement model.graph)
            |> g [ class [ "links" ] ]
        , Graph.nodes model.graph
            |> List.map nodeElement
            |> g [ class [ "nodes" ] ]
        ]






{- {"delay": 5001} -}
