port module NetworkSimulator exposing (main, Msg(..))

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
import IntDict
import Network exposing (Entity, Status(..), NodeState)
import Element.Border as Border
import Html exposing (Html)
import Html.Events.Extra.Mouse as Mouse
import Json.Decode as Decode
import Json.Encode as Encode
import Time
import TypedSvg exposing (circle, g, line, svg, title, text_)
import TypedSvg.Attributes exposing (class, fill, stroke, viewBox, fontSize, transform)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core as Svg exposing (Attribute, Svg)
import TypedSvg.Types exposing (Fill(..), Length(..), Transform(..))
import Random
import List.Extra
import CellGrid exposing (CellGrid, CellRenderer)
import Grid


gameTimeInterval =
    1000


searchForInfluencersInterval =
    4


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



--
-- MODEL
--


type GraphBehavior
    = Selectable
    | Draggable


type alias Model =
    { drag : Maybe Drag
    , recruiter : NodeId
    , clickCount : Int
    , graph : Graph Entity ()
    , hiddenGraph : Graph Entity ()
    , graphBehavior : GraphBehavior
    , simulation : Force.State NodeId
    , message : String
    , gameClock : Int
    , gameState : GameState
    , randomNumberList : List Float
    , displayMode : DisplayMode
    , grid : CellGrid Grid.Cell
    }


type DisplayMode
    = DisplayGraph
    | DisplayGrid


type GameState
    = Ready
    | Running
    | Paused
    | GameOver


type alias Drag =
    { start : ( Float, Float )
    , current : ( Float, Float )
    , index : NodeId
    }


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
          , graphBehavior = Selectable
          , simulation = (Force.simulation forces)
          , message = "No message yet"
          , gameClock = 0
          , gameState = Ready
          , randomNumberList = []
          , displayMode = DisplayGrid
          , grid = Grid.cellGridFromGraph gridWidth graph -- Grid.empty gridWidth gridWidth
          }
        , Cmd.none
        )


type AudioMessage
    = Silence
    | Chirp
    | Coo
    | LongChirp


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


port sendMessage : Encode.Value -> Cmd msg


sendAudioMessage : AudioMessage -> Cmd msg
sendAudioMessage audioMsg =
    sendMessage <| encodeAudioMessage audioMsg


updateNode : ( Float, Float ) -> NodeContext Entity () -> NodeContext Entity ()
updateNode ( x, y ) nodeCtx =
    let
        nodeValue =
            nodeCtx.node.label
    in
        Network.updateContextWithValue nodeCtx { nodeValue | x = x, y = y }


updateGraphWithList : Graph Entity () -> List Entity -> Graph Entity ()
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
            let
                ( newState, list ) =
                    Force.tick model.simulation <| List.map .label <| Graph.nodes model.graph
            in
                case model.drag of
                    Nothing ->
                        { model | graph = updateGraphWithList model.graph list, simulation = newState } |> putCmd Cmd.none

                    Just { current, index } ->
                        { model | graph = (Graph.update index (Maybe.map (updateNode current)) (updateGraphWithList model.graph list)) }
                            |> putCmd Cmd.none

        GameTick t ->
            case model.gameState of
                Running ->
                    ( { model | gameClock = model.gameClock + 1 }, getRandomNumbers )

                _ ->
                    ( model, getRandomNumbers )

        DragStart index xy ->
            { model | drag = Just (Drag xy xy index) } |> putCmd Cmd.none

        MouseClick index xy ->
            let
                associatedIncomingNodeIds =
                    (Network.inComingNodeIds index model.hiddenGraph)

                associatedOutgoingNodeIds =
                    (Network.outGoingNodeIds index model.hiddenGraph)

                newGraph =
                    Network.setStatus index Recruited model.graph
                        |> Network.connect model.recruiter index
                        |> Network.connectNodeToNodeInList model.recruiter associatedOutgoingNodeIds

                newGrid =
                    Grid.cellGridFromGraph gridWidth newGraph

                forces =
                    Network.computeForces newGraph
            in
                { model
                    | graph = newGraph
                    , grid = newGrid
                    , simulation = (Force.simulation forces)
                    , clickCount = model.clickCount + 1
                }
                    |> putCmd Cmd.none

        DragAt xy ->
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

        DragEnd xy ->
            case model.drag of
                Just { start, index } ->
                    { model | drag = Nothing, graph = Graph.update index (Maybe.map (updateNode xy)) model.graph } |> putCmd Cmd.none

                Nothing ->
                    model |> putCmd Cmd.none

        SetGraphBehavior behavior ->
            { model | graphBehavior = behavior } |> putCmd Cmd.none

        AdvanceGameState ->
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
                            Running
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
                                , simulation = (Force.simulation forces)
                                , clickCount = 0
                                , gameClock = 0
                                , gameState = newGameState
                            }
                                |> putCmd Cmd.none

                    _ ->
                        { model | gameState = newGameState } |> putCmd Cmd.none

        ReHeat ->
            { model | simulation = Force.reheat model.simulation } |> putCmd Cmd.none

        GetRandomNumbers ->
            ( model, getRandomNumbers )

        GotRandomNumbers numbers ->
            let
                newGraph1 =
                    -- Recruit a new node
                    case model.gameState == Running && modBy searchForInfluencersInterval model.gameClock == 0 of
                        True ->
                            Network.recruitNodes numbers model.recruiter model.graph model.hiddenGraph

                        False ->
                            model.graph

                newGraph2 =
                    case model.gameState == Running && modBy 41 model.gameClock == 0 && Network.influencees model.recruiter newGraph1 /= [] of
                        False ->
                            newGraph1

                        True ->
                            let
                                freeNodes =
                                    Network.nodeComplementOfGraph newGraph1
                                        ((Network.influencees model.recruiter newGraph1) ++ [ model.recruiter ])

                                rn2 =
                                    List.Extra.getAt 2 numbers

                                freeNode =
                                    Network.randomListElement rn2 freeNodes
                            in
                                case freeNode of
                                    Nothing ->
                                        newGraph1

                                    Just nodeId_ ->
                                        Network.connect model.recruiter nodeId_ newGraph1
                                            |> Network.setStatus nodeId_ Recruited

                newGameState =
                    case List.length (Network.influencees model.recruiter newGraph2) == Graph.size newGraph2 - 1 of
                        True ->
                            GameOver

                        False ->
                            model.gameState

                newGrid =
                    Grid.cellGridFromGraph gridWidth newGraph2
            in
                ( { model
                    | randomNumberList = numbers
                    , graph = newGraph2
                    , grid = newGrid
                    , gameState = newGameState
                  }
                , Cmd.none
                  -- sendAudioMessage audioMsg
                )

        SetDisplayMode displayMode ->
            ( { model | displayMode = displayMode }, Cmd.none )

        ResetGame ->
            let
                ( forces, graph ) =
                    Network.setupGraph Network.testGraph
            in
                ( { model
                    | gameState = Ready
                    , gameClock = 0
                    , graph = graph
                    , grid = Grid.cellGridFromGraph gridWidth graph
                  }
                , Cmd.none
                )

        CellGrid msg_ ->
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
                            (Network.outGoingNodeIds index model.hiddenGraph)

                        audioMsg =
                            case List.length associatedOutgoingNodeIds == 0 of
                                True ->
                                    Chirp

                                False ->
                                    LongChirp

                        newGraph =
                            Network.setStatus index Recruited model.graph
                                |> Network.connect model.recruiter index
                                |> Network.connectNodeToNodeInList model.recruiter associatedOutgoingNodeIds

                        newGrid =
                            Grid.cellGridFromGraph gridWidth newGraph

                        message =
                            "CHIRP1, Grid, node = " ++ String.fromInt index ++ ", (i,j) = (" ++ String.fromInt i ++ ", " ++ String.fromInt j ++ ")"
                    in
                        ( { model | message = message, graph = newGraph, grid = newGrid }, sendAudioMessage audioMsg )


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


linkElement :
    Graph (Force.Entity Int { value : NodeState }) e
    -> { a | from : Graph.NodeId, to : Graph.NodeId }
    -> Svg msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 Network.defaultNodeState) <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 Network.defaultNodeState) <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
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


nodeElement : Model -> Node { a | value : NodeState, x : Float, y : Float } -> Svg Msg
nodeElement model node =
    let
        mouseHandler =
            case model.graphBehavior of
                Selectable ->
                    onMouseClick

                Draggable ->
                    onMouseDown
    in
        g []
            [ circle
                [ r 14.0
                , if node.label.value.status == NotRecruited then
                    fill (Fill (Color.rgba 0.3 0 1 1.0))
                  else
                    fill (Fill (Color.rgba 1.0 0 0.3 1.0))
                , stroke (Color.rgba 0 0 0 0)
                , strokeWidth 7
                , mouseHandler node.id
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
    column [ spacing 12, centerX, centerY ]
        [ row [ spacing 24 ]
            [ leftPanel model
            , rightPanel model
            ]
        ]


leftPanel : Model -> Element Msg
leftPanel model =
    column [ spacing 12, width (px 500), padding 40 ]
        [ infoPanel model
        , controlPanel model
        ]


infoPanel : Model -> Element Msg
infoPanel model =
    column [ spacing 12, width (px 450), padding 40, Border.width 1 ]
        [ el [ alignTop ] (text "SIMULATION")
        , el [ Font.size 14 ] (text "Click on nodes to 'recruit' them.")
        , el [ Font.size 14 ] (text "Clicking certain nodes will automatically recruit others.")
        , el [ Font.size 14 ] (text "Why?")
        , row [ spacing 18 ] [ displayGraphButton model, displayGridButton model ]
        , row [ Font.size 12 ] [ el [] (text model.message) ]
        ]


controlPanel : Model -> Element Msg
controlPanel model =
    column [ spacing 12, width (px 450), padding 40, Border.width 1, Font.size 16 ]
        [ clockIndicator model
        , scoreIndicator model
        , row [ spacing 18 ]
            [ el [] (text <| "Nodes: " ++ (String.fromInt <| Graph.size model.graph))
            , el [] (text <| "Recruited: " ++ (String.fromInt (List.length <| recruitedNodes model)))
            ]

        -- , row [spacing 12] [ enableSelectionButton model, enableDragginButton model]
        , row [ spacing 18 ]
            [ startOverButton model
            , resetButton model
            ]
        , influenceesDisplay model

        -- , influenceesDisplay2 model
        ]


influenceesDisplay : Model -> Element Msg
influenceesDisplay model =
    let
        ii =
            Network.influencees model.recruiter model.graph
                |> List.map String.fromInt
                |> String.join ", "
    in
        el [] (text <| "Influencees: " ++ ii)


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
        ]


viewGrid : Model -> Float -> Float -> Html Msg
viewGrid model w h =
    CellGrid.renderAsHtml 500 500 cellRenderer model.grid |> Html.map CellGrid


cellRenderer =
    { cellSize = 25
    , cellColorizer =
        \cell ->
            case cell.status of
                Grid.Recruited ->
                    Color.rgb 1 0 0

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
        [ Graph.edges model.graph
            |> List.map (linkElement model.graph)
            |> g [ class [ "links" ] ]
        , Graph.nodes model.graph
            |> List.map (nodeElement model)
            |> g [ class [ "nodes" ] ]
        ]



--
-- BUTTONS
--


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
        { onPress = Just (AdvanceGameState)
        , label = el [] (text <| controlButtonTitle model)
        }


resetButton : Model -> Element Msg
resetButton model =
    Input.button (buttonStyle [ Background.color charcoal ])
        { onPress = Just (ResetGame)
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

        GameOver ->
            "Play again"


reheatButton : Model -> Element Msg
reheatButton model =
    Input.button (buttonStyle [ Background.color charcoal ])
        { onPress = Just (ReHeat)
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