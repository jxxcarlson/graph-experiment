port module NetworkSimulator exposing (Msg(..), cellRenderer, main, stringForGraphEdges)

{-| This demonstrates laying out the characters in Les Miserables
based on their co-occurence in a scene. Try dragging the nodes!
-}

import Browser
import Browser.Events
import CellGrid exposing (CellGrid, CellRenderer)
import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Grid
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import IntDict
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Network exposing (EdgeLabel, Entity, NodeState, Status(..), creditNode, moneySupply)
import NetworkMeasure as NM
import Random
import SimpleGraph
import Time
import TypedSvg exposing (circle, g, line, rect, svg, text_, title)
import TypedSvg.Attributes exposing (class, fill, fontSize, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx as Apx exposing (cx, cy, r, strokeWidth, x, x1, x2, y, y1, y2)
import TypedSvg.Core as Svg exposing (Attribute, Svg)
import TypedSvg.Types exposing (Fill(..), Length(..), Transform(..))


gameTimeInterval =
    1000


recruitInterval =
    8


recruitStep =
    0



-- recruitInterval // 2


transactionStep =
    1


gridWidth =
    20


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> update msg model
        , subscriptions = subscriptions
        }



--
-- MSG
--


type Msg
    = DragStart NodeId ( Float, Float )
    | MouseClick NodeId ( Float, Float )
    | DragAt ( Float, Float )
    | DragEnd ( Float, Float )
    | Tick Time.Posix
    | GameTick Time.Posix
    | SetGraphBehavior GraphBehavior
    | ReHeat
    | AdvanceGameState
    | ResetGame
    | GetRandomNumbers
    | GotRandomNumbers (List Float)
    | SetDisplayMode DisplayMode
    | CellGrid CellGrid.Msg


type GraphBehavior
    = Selectable
    | Draggable



-- MODEL --


type alias Measures =
    { sustainability : Float
    , totalFlow : Float
    , resilience : Float
    , efficiency : Float
    , gini : Float
    }


measures : Model -> Measures
measures model =
    let
        sg =
            Network.simplifyGraph model.graph
    in
    { sustainability = NM.sustainabilityPercentage sg |> NM.roundTo 2
    , totalFlow = NM.totalFlow sg |> NM.roundTo 2
    , resilience = NM.resilience sg |> NM.roundTo 2
    , efficiency = NM.efficiency sg |> NM.roundTo 2
    , gini = NM.giniIndex sg |> NM.roundTo 2
    }


type alias Model =
    { drag : Maybe Drag
    , recruiter : NodeId
    , clickCount : Int
    , graph : Graph Entity EdgeLabel
    , hiddenGraph : Graph Entity EdgeLabel
    , numberOfTransactionsToDate : Int
    , graphBehavior : GraphBehavior
    , simulation : Force.State NodeId
    , message : String
    , gameClock : Int
    , gameState : GameState
    , randomNumberList : List Float
    , displayMode : DisplayMode
    , grid : CellGrid Grid.Cell
    , history : List Measures
    }


type DisplayMode
    = DisplayGraph
    | DisplayGrid


type GameState
    = Ready
    | Running
    | Paused
    | GameEnding
    | GameOver


type alias Drag =
    { start : ( Float, Float )
    , current : ( Float, Float )
    , index : NodeId
    }



-- g1 = setupGraph testGraph |> Tuple.second


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( forces, graph ) =
            Network.setupGraph Network.testGraph

        hiddenGraph =
            Graph.mapContexts Network.initializeNode Network.hiddenTestGraph
    in
    ( { drag = Nothing
      , graph = graph
      , recruiter = 12
      , clickCount = 0
      , hiddenGraph = hiddenGraph
      , numberOfTransactionsToDate = 0
      , graphBehavior = Selectable
      , simulation = Force.simulation forces
      , message = "No message yet"
      , gameClock = 0
      , gameState = Ready
      , randomNumberList = []
      , displayMode = DisplayGraph
      , grid = Grid.cellGridFromGraph gridWidth graph -- Grid.empty gridWidth gridWidth
      , history = []
      }
    , Cmd.none
    )


type AudioMessage
    = Silence
    | Chirp
    | Coo
    | LongChirp
    | VeryLongChirp


encodeAudioMessage : AudioMessage -> Encode.Value
encodeAudioMessage msg =
    case msg of
        Silence ->
            Encode.string "silence"

        Chirp ->
            Encode.string "chirp"

        Coo ->
            Encode.string "coo"

        LongChirp ->
            Encode.string "longChirp"

        VeryLongChirp ->
            Encode.string "veryLongChirp"


port sendMessage : Encode.Value -> Cmd msg


sendAudioMessage : AudioMessage -> Cmd msg
sendAudioMessage audioMsg =
    sendMessage <| encodeAudioMessage audioMsg


updateNode : ( Float, Float ) -> NodeContext Entity EdgeLabel -> NodeContext Entity EdgeLabel
updateNode ( x, y ) nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
    Network.updateContextWithValue nodeCtx { nodeValue | x = x, y = y }


updateGraphWithList : Graph Entity EdgeLabel -> List Entity -> Graph Entity EdgeLabel
updateGraphWithList =
    let
        graphUpdater value =
            Maybe.map (\ctx -> Network.updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


putCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
putCmd cmd model =
    ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            handleTick model t

        GameTick t ->
            handleGameTick model t

        DragStart index xy ->
            { model | drag = Just (Drag xy xy index) } |> putCmd Cmd.none

        MouseClick index xy ->
            handleMouseClick model index xy

        DragAt xy ->
            handleDragAt model xy

        DragEnd xy ->
            handleDragEnd model xy

        SetGraphBehavior behavior ->
            { model | graphBehavior = behavior } |> putCmd Cmd.none

        AdvanceGameState ->
            advanceGameState model

        ReHeat ->
            { model | simulation = Force.reheat model.simulation } |> putCmd Cmd.none

        GetRandomNumbers ->
            ( model, getRandomNumbers )

        GotRandomNumbers numbers ->
            randomUpdate model numbers

        SetDisplayMode displayMode ->
            ( { model | displayMode = displayMode }, Cmd.none )

        ResetGame ->
            resetGame model

        CellGrid msg_ ->
            handleMouseClickInGrid model msg_



-- UPDATE HELPERS --


handleTick model t =
    let
        ( newState, list ) =
            Force.tick model.simulation <| List.map .label <| Graph.nodes model.graph
    in
    case model.drag of
        Nothing ->
            { model | graph = updateGraphWithList model.graph list, simulation = newState } |> putCmd Cmd.none

        Just { current, index } ->
            { model | graph = Graph.update index (Maybe.map (updateNode current)) (updateGraphWithList model.graph list) }
                |> putCmd Cmd.none


handleGameTick model t =
    case model.gameState of
        Running ->
            ( { model | gameClock = model.gameClock + 1 }, getRandomNumbers )

        _ ->
            ( model, getRandomNumbers )


handleMouseClick model index xy =
    if model.gameState /= Running then
        ( model, Cmd.none )

    else
        let
            associatedIncomingNodeIds =
                Network.inComingNodeIds index model.hiddenGraph

            associatedOutgoingNodeIds =
                Network.outGoingNodeIds index model.hiddenGraph

            audioMsg =
                case List.length associatedOutgoingNodeIds == 0 of
                    True ->
                        Chirp

                    False ->
                        LongChirp

            newGraph =
                Network.setStatus index Recruited model.graph
                    |> Network.creditNode model.recruiter 1
                    |> Network.changeAccountBalance index 10
                    |> Network.connect model.recruiter index
                    |> Network.incrementRecruitedCount model.recruiter
                    |> Network.connectNodeToNodeInList model.recruiter associatedOutgoingNodeIds

            newGrid =
                Grid.cellGridFromGraph gridWidth newGraph

            forces =
                Network.computeForces newGraph
        in
        { model
            | graph = newGraph
            , grid = newGrid
            , simulation = Force.simulation forces
            , clickCount = model.clickCount + 1
            , message = "Recruit node " ++ String.fromInt index
        }
            |> putCmd (sendAudioMessage audioMsg)


handleDragAt model xy =
    case model.drag of
        Just { start, index } ->
            { model
                | drag = Just (Drag start xy index)
                , graph = Graph.update index (Maybe.map (updateNode xy)) model.graph
                , simulation = Force.reheat model.simulation
            }
                |> putCmd Cmd.none

        Nothing ->
            model |> putCmd Cmd.none


handleDragEnd model xy =
    case model.drag of
        Just { start, index } ->
            { model | drag = Nothing, graph = Graph.update index (Maybe.map (updateNode xy)) model.graph } |> putCmd Cmd.none

        Nothing ->
            model |> putCmd Cmd.none


advanceGameState model =
    let
        newGameState =
            case model.gameState of
                Ready ->
                    Running

                Running ->
                    Paused

                Paused ->
                    Running

                GameOver ->
                    Ready

                GameEnding ->
                    GameOver
    in
    case model.gameState of
        GameOver ->
            let
                ( forces, graph ) =
                    Network.setupGraph Network.testGraph
            in
            { model
                | graph = graph
                , grid = Grid.cellGridFromGraph gridWidth graph
                , simulation = Force.simulation forces
                , clickCount = 0
                , gameClock = 0
                , gameState = newGameState
            }
                |> putCmd Cmd.none

        _ ->
            { model | gameState = newGameState } |> putCmd Cmd.none


randomUpdate model numbers =
    let
        recruitCount1 =
            Grid.recruitedCount model.grid

        nextHistory =
            if modBy recruitInterval model.gameClock == 1 then
                measures model :: model.history

            else
                model.history

        ( deltaRecuiterAccount, newGraph1 ) =
            -- New recruitees recruit other nodes at random
            case
                model.gameState
                    == Running
                    && modBy recruitInterval model.gameClock
                    == recruitStep
            of
                False ->
                    ( 0, model.graph )

                True ->
                    ( 1, Network.recruitRandom numbers model.recruiter model.graph )

        ( transactionRecord, newGraph ) =
            case modBy recruitInterval model.gameClock == transactionStep of
                False ->
                    ( Nothing, newGraph1 )

                True ->
                    Network.randomTransaction (List.head numbers) (List.head (List.drop 1 numbers)) 1 newGraph1

        ( message, deltaTransactions ) =
            case transactionRecord of
                Nothing ->
                    ( model.message, 0 )

                Just ( i, j ) ->
                    ( "Transfer 1 unit from node " ++ String.fromInt i ++ " to node " ++ String.fromInt j, 1 )

        newGrid =
            Grid.cellGridFromGraph gridWidth newGraph

        recruitCount2 =
            Grid.recruitedCount newGrid

        newGameState =
            let
                everyoneRecruited =
                    Grid.recruitedCount model.grid == Graph.size model.graph
            in
            case ( model.gameState, everyoneRecruited ) of
                ( Running, True ) ->
                    GameEnding

                ( GameEnding, _ ) ->
                    GameOver

                _ ->
                    model.gameState

        audioMsg =
            case ( newGameState, recruitCount2 - recruitCount1 > 0 ) of
                ( GameEnding, _ ) ->
                    VeryLongChirp

                ( Running, True ) ->
                    Coo

                _ ->
                    Silence
    in
    ( { model
        | randomNumberList = numbers
        , graph = newGraph
        , grid = newGrid
        , gameState = newGameState
        , message = message
        , history = nextHistory
        , numberOfTransactionsToDate = model.numberOfTransactionsToDate + deltaTransactions
      }
    , sendAudioMessage audioMsg
    )


resetGame model =
    let
        ( forces, graph ) =
            Network.setupGraph Network.testGraph
    in
    ( { model
        | gameState = Ready
        , gameClock = 0
        , clickCount = 0
        , history = []
        , graph = graph
        , simulation = Force.simulation forces
        , grid = Grid.cellGridFromGraph gridWidth graph
      }
    , Cmd.none
    )


handleMouseClickInGrid model msg_ =
    if model.gameState /= Running then
        ( model, Cmd.none )

    else
        case msg_ of
            CellGrid.MouseClick ( i, j ) ( x, y ) ->
                let
                    index =
                        case CellGrid.cellAtMatrixIndex ( i, j ) model.grid of
                            Nothing ->
                                -1

                            Just cell ->
                                cell.id

                    associatedOutgoingNodeIds =
                        Network.outGoingNodeIds index model.hiddenGraph

                    audioMsg =
                        case List.length associatedOutgoingNodeIds == 0 of
                            True ->
                                Chirp

                            False ->
                                LongChirp

                    newGraph =
                        Network.setStatus index Recruited model.graph
                            |> Network.changeAccountBalance index 10
                            |> Network.connect model.recruiter index
                            |> Network.incrementRecruitedCount model.recruiter
                            |> Network.connectNodeToNodeInList model.recruiter associatedOutgoingNodeIds

                    newGrid =
                        Grid.cellGridFromGraph gridWidth newGraph

                    message =
                        "cellGrid: mouse click"
                in
                ( { model | message = message, graph = newGraph, grid = newGrid }, sendAudioMessage audioMsg )



-- /UPDATE HELPERS --


getRandomNumbers : Cmd Msg
getRandomNumbers =
    Random.generate GotRandomNumbers (Random.list 10 (Random.float 0 1))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ simulationSubscription model, Time.every gameTimeInterval GameTick ]


simulationSubscription : Model -> Sub Msg
simulationSubscription model =
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



-- Mouse.onDown (.clientPos >> MouseClick index)


onMouseClick : NodeId -> Attribute Msg
onMouseClick index =
    Mouse.onClick (.clientPos >> MouseClick index)



-- GRAPH VIEW HELPERS --


linkElement :
    Graph (Force.Entity Int { value : NodeState }) e
    -> Edge EdgeLabel
    -> Svg msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 Network.defaultNodeState) <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 Network.defaultNodeState) <| Maybe.map (.node >> .label) <| Graph.get edge.to graph

        color =
            case Network.absoluteEdgeFlow edge of
                0 ->
                    Color.rgb255 255 255 255

                _ ->
                    Color.rgb255 0 0 255
    in
    line
        [ strokeWidth (toFloat <| Network.absoluteEdgeFlow edge + 1)
        , stroke color
        , x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        ]
        []


nodeElement : Model -> Node { a | value : NodeState, x : Float, y : Float } -> Svg Msg
nodeElement model node =
    let
        mouseHandler =
            case model.graphBehavior of
                Selectable ->
                    onMouseClick

                Draggable ->
                    onMouseDown

        accBal : Float
        accBal =
            node.label.value.accountBalance |> toFloat
    in
    g []
        [ circle
            [ r <| 12.0 + (accBal / 2.5)
            , nodeColorizer node
            , stroke (Color.rgba 0 0 0 0)
            , strokeWidth 7
            , mouseHandler node.id
            , cx node.label.x
            , cy node.label.y
            ]
            [ title [] [ Svg.text node.label.value.name ] ]
        , text_
            [ transform [ Translate (node.label.x - 6) (node.label.y + 3) ]
            , fontSize (Px 12)
            , stroke (Color.rgba 1 1 1 1)
            ]
            [ Svg.text (String.fromInt node.label.value.accountBalance) ]
        ]


nodeColorizer node =
    case node.label.value.status of
        NotRecruited ->
            TypedSvg.Attributes.fill (Fill (Color.rgb255 0 0 255))

        Recruited ->
            case node.label.value.parentGraphId of
                100 ->
                    TypedSvg.Attributes.fill (Fill (Color.rgb255 244 65 238))

                0 ->
                    TypedSvg.Attributes.fill (Fill (Color.rgb255 0 180 0))

                1 ->
                    TypedSvg.Attributes.fill (Fill (Color.rgb255 244 128 65))

                _ ->
                    TypedSvg.Attributes.fill (Fill (Color.rgb255 244 65 238))



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column [ spacing 12, centerX, centerY ]
        [ row [ spacing 24 ]
            [ leftPanel model
            , rightPanel model
            ]

        --, el [ Font.size 14 ] (displayEdges model.graph)
        ]


displayEdges : Graph Entity EdgeLabel -> Element Msg
displayEdges g =
    paragraph [ Font.size 14, spacing 8 ] [ text <| stringForGraphEdges g ]


stringForGraphEdges : Graph Entity EdgeLabel -> String
stringForGraphEdges g =
    g
        |> Graph.edges
        |> List.sortBy (\edge -> 1000 * edge.from + edge.to)
        -- |> List.filter (\edge -> edge.label.unitsSent /= 0)
        |> List.map stringFromEdge
        |> String.join " ... "


stringFromEdge : Edge EdgeLabel -> String
stringFromEdge edge =
    String.fromInt edge.from
        ++ " -> "
        ++ String.fromInt edge.to
        ++ " ("
        ++ String.fromInt edge.label.unitsSent
        ++ ", "
        ++ String.fromInt edge.label.unitsReceived
        ++ ")"


leftPanel model =
    column [ spacing 12, width (px 500), padding 40 ]
        [ infoPanel model
        , controlPanel model
        ]


infoPanel : Model -> Element Msg
infoPanel model =
    column [ spacing 12, width (px 450), padding 40, Border.width 1 ]
        [ el [ alignTop ] (text "SIMULATION")
        , el [ Font.size 14 ] (text "Press 'Ready', then click on nodes to 'recruit' them.")
        , row [ spacing 18 ] [ displayGraphButton model, displayGridButton model ]
        ]


controlPanel : Model -> Element Msg
controlPanel model =
    column [ spacing 12, width (px 450), padding 40, Border.width 1, Font.size 16 ]
        [ clockIndicator model
        , scoreIndicator model
        , row [ spacing 18 ]
            [ el [] (text <| "Nodes: " ++ (String.fromInt <| Graph.size model.graph))
            , recruitedDisplay model

            -- , el [] (text <| )
            ]

        -- , row [spacing 12] [ enableSelectionButton model, enableDragginButton model]
        , row [ spacing 18 ]
            [ startOverButton model
            , resetButton model
            ]
        , accountDisplay model
        , row [] [ el [] (text <| "Number of transactions: " ++ String.fromInt model.numberOfTransactionsToDate) ]
        , row [ Font.size 12 ] [ el [] (text model.message) ]
        , displayMeasures model
        ]



-- DISPLAY MEASURES --


displayMeasures : Model -> Element Msg
displayMeasures model =
    let
        m =
            measures model
    in
    row [ spacing 12 ]
        [ el [] (text <| "tf: " ++ String.fromFloat m.totalFlow)
        , el [] (text <| "gini: " ++ String.fromFloat m.gini)
        , el [] (text <| "sus: " ++ String.fromFloat m.sustainability)
        , el [] (text <| "eff: " ++ String.fromFloat m.efficiency)
        , el [] (text <| "res: " ++ String.fromFloat m.resilience)
        ]



-- DISPLAY ACCOUNTS AND MONEY SUPPLY --


accountDisplay : Model -> Element Msg
accountDisplay model =
    column [ paddingXY 0 12 ]
        [ row [ moveLeft 48 ] [ accountChart model.graph ]
        , row [ spacing 18 ]
            [ moneySupplyDisplay model
            , numberOfTradersDisplay model
            ]
        ]


moneySupplyDisplay : Model -> Element Msg
moneySupplyDisplay model =
    let
        moneySupply =
            Network.moneySupply model.graph
    in
    el [] (text <| "Money supply = " ++ String.fromInt (Network.moneySupply model.graph))


numberOfTradersDisplay : Model -> Element Msg
numberOfTradersDisplay model =
    let
        nodeFilter : Entity -> Bool
        nodeFilter entity =
            (Network.nodeState entity).accountBalance > 0

        n =
            List.length <| Network.filterNodes nodeFilter model.graph
    in
    el [] (text <| "Trading population = " ++ String.fromInt n)



-- CHARTS --


accountChart : Graph Entity EdgeLabel -> Element Msg
accountChart graph =
    let
        data =
            Network.accountList graph
                |> List.map (Tuple.second >> toFloat)
                |> List.reverse
    in
    SimpleGraph.barChart barGraphAttributes data |> Element.html


sustainabilityChart : Model -> Element Msg
sustainabilityChart model =
    let
        data =
            List.map .sustainability model.history
                |> List.take 100
                |> List.reverse
    in
    SimpleGraph.barChart wideBarGraphAttributes data |> Element.html


giniChart : Model -> Element Msg
giniChart model =
    let
        data =
            List.map .gini model.history
                |> List.take 100
                |> List.reverse
    in
    SimpleGraph.barChart wideBarGraphAttributes data |> Element.html


barGraphAttributes =
    { graphHeight = 70
    , graphWidth = 300
    , options = [ SimpleGraph.Color "rgb(200,0,0)", SimpleGraph.DeltaX 15, SimpleGraph.YTickmarks 6, SimpleGraph.XTickmarks 2 ]
    }


wideBarGraphAttributes =
    { graphHeight = 35
    , graphWidth = 420
    , options = [ SimpleGraph.Color "rgb(200,0,0)", SimpleGraph.DeltaX 4, SimpleGraph.YTickmarks 6, SimpleGraph.XTickmarks 2 ]
    }



-- DISPLAY INFLUENCER --


influenceesDisplay : Model -> Element Msg
influenceesDisplay model =
    let
        ii =
            Network.influencees model.recruiter model.graph
                |> List.map String.fromInt
                |> String.join ", "
    in
    el [] (text <| "Influencees: " ++ ii)


recruitedDisplay : Model -> Element Msg
recruitedDisplay model =
    let
        n =
            Grid.recruitedCount model.grid - 1 |> String.fromInt
    in
    el [] (text <| "Recruited: " ++ n)


influenceesDisplay2 : Model -> Element Msg
influenceesDisplay2 model =
    let
        ii =
            Network.influencees2 4 model.hiddenGraph
                |> List.map String.fromInt
                |> String.join ", "
    in
    el [] (text <| "Influencees2 of p4: " ++ ii)


clockIndicator : Model -> Element Msg
clockIndicator model =
    case model.gameState of
        GameOver ->
            el [ Font.size 24, Font.bold ] (text <| "Game Over!")

        _ ->
            el [ Font.size 24, Font.bold ] (text <| "Clock: " ++ String.fromInt model.gameClock)


scoreIndicator : Model -> Element Msg
scoreIndicator model =
    let
        cc =
            toFloat <| model.clickCount

        gc =
            toFloat <| model.gameClock

        rn =
            toFloat <| List.length <| recruitedNodes model

        score =
            round <| (30 * rn - 20 * cc - 2.5 * gc)
    in
    el [ Font.size 24, Font.bold ] (text <| "Score: " ++ String.fromInt score)


recruitedNodes : Model -> List NodeId
recruitedNodes model =
    Network.outGoingNodeIds model.recruiter model.graph


rightPanel : Model -> Element Msg
rightPanel model =
    column [ spacing 12, width (px 500), Border.width 1 ]
        [ case model.displayMode of
            DisplayGraph ->
                viewGraph model 500 500 |> Element.html

            DisplayGrid ->
                viewGrid model 500 500 |> Element.html
        , sustainabilityChart model
        , giniChart model
        ]


viewGrid : Model -> Float -> Float -> Html Msg
viewGrid model w h =
    CellGrid.renderAsHtml 500 500 cellRenderer model.grid |> Html.map CellGrid


cellRenderer : CellRenderer Grid.Cell
cellRenderer =
    { cellSize = 25
    , cellColorizer =
        \cell ->
            case cell.status of
                Grid.Recruited ->
                    case cell.parentGraphId of
                        100 ->
                            Color.rgb255 244 65 238

                        0 ->
                            Color.rgb255 66 244 137

                        1 ->
                            Color.rgb255 244 128 65

                        _ ->
                            Color.rgb255 244 65 238

                Grid.NotRecruited ->
                    Color.rgb 0 0 1

                Grid.Vacant ->
                    Color.rgb 0 0 0
    , text = Just (\cell -> cell.name)
    , defaultColor = Color.rgb 0 0 0
    , gridLineWidth = 0.5
    , gridLineColor = Color.rgb 0.5 0.5 0.5
    }


viewGraph : Model -> Float -> Float -> Html Msg
viewGraph model w h =
    svg [ viewBox 0 0 w h ]
        [ rect [ x 0, y 0, Apx.width w, Apx.height h ] []
        , Graph.edges model.graph
            |> List.map (linkElement model.graph)
            |> g [ class [ "links" ] ]
        , Graph.nodes model.graph
            |> List.map (nodeElement model)
            |> g [ class [ "nodes" ] ]
        ]



-- BUTTONS --


displayGraphButton : Model -> Element Msg
displayGraphButton model =
    Input.button (buttonStyle [ activeBackground (model.displayMode == DisplayGraph) ])
        { onPress = Just (SetDisplayMode DisplayGraph)
        , label = el [ Font.size 14 ] (text "Graph")
        }


activeBackground flag =
    case flag of
        True ->
            Background.color darkRed

        False ->
            Background.color charcoal


displayGridButton : Model -> Element Msg
displayGridButton model =
    Input.button (buttonStyle [ activeBackground (model.displayMode == DisplayGrid) ])
        { onPress = Just (SetDisplayMode DisplayGrid)
        , label = el [ Font.size 14 ] (text "Grid")
        }


startOverButton : Model -> Element Msg
startOverButton model =
    Input.button (buttonStyle [ Background.color charcoal ])
        { onPress = Just AdvanceGameState
        , label = el [] (text <| controlButtonTitle model)
        }


resetButton : Model -> Element Msg
resetButton model =
    Input.button (buttonStyle [ Background.color charcoal ])
        { onPress = Just ResetGame
        , label = el [] (text <| "Reset")
        }


controlButtonTitle : Model -> String
controlButtonTitle model =
    case model.gameState of
        Ready ->
            "Ready"

        Running ->
            "Running"

        Paused ->
            "Paused"

        GameEnding ->
            "Game ending"

        GameOver ->
            "Play again"


reheatButton : Model -> Element Msg
reheatButton model =
    Input.button (buttonStyle [ Background.color charcoal ])
        { onPress = Just ReHeat
        , label = el [] (text "Arrange")
        }


enableSelectionButton : Model -> Element Msg
enableSelectionButton model =
    Input.button (buttonStyle [ selectedBackground (model.graphBehavior == Selectable) ])
        { onPress = Just (SetGraphBehavior Selectable)
        , label = el [] (text "Select Nodes")
        }


enableDragginButton : Model -> Element Msg
enableDragginButton model =
    Input.button (buttonStyle [ selectedBackground (model.graphBehavior == Draggable) ])
        { onPress = Just (SetGraphBehavior Draggable)
        , label = el [] (text "Move Nodes")
        }


buttonStyle extras =
    [ Border.width 1, padding 8, Border.rounded 4, Font.color white ] ++ extras


white =
    rgb 1 1 1


charcoal =
    rgb 0.3 0.3 0.3


darkRed =
    rgb 0.6 0 0


selectedBackground flag =
    case flag of
        True ->
            Background.color darkRed

        False ->
            Background.color charcoal
