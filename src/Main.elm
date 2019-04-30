module Main exposing (main, computeForces)

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
import Network exposing(Entity, Status(..), NodeState)
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




--connectNodes : Int -> Int -> Graph Entity () -> Graph Entity ()
--connectNodes from to graph =
--   Graph.mapEdges


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }

--
-- MSG
--

type Msg
    = DragStart NodeId ( Float, Float )
    | MouseClick  NodeId ( Float, Float )
    | DragAt ( Float, Float )
    | DragEnd ( Float, Float )
    | Tick Time.Posix
    | SetGraphBehavior GraphBehavior
    | ReHeat
    | StartOver


--
-- MODEL
--

type GraphBehavior = Selectable | Draggable

type alias Model =
    { drag : Maybe Drag
    , recruiter : NodeId
    , clickCount : Int
    , graph : Graph Entity ()
    , hiddenGraph : Graph Entity ()
    , graphBehavior : GraphBehavior
    , simulation : Force.State NodeId
    , message : String
    }


type alias Drag =
    { start : ( Float, Float )
    , current : ( Float, Float )
    , index : NodeId
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        (forces, graph) = setupGraph Network.testGraph
        hiddenGraph =  Graph.mapContexts Network.initializeNode Network.hiddenTestGraph
    in
      ({ drag = Nothing
      , graph = graph
      , recruiter = 12
      , clickCount = 0
      , hiddenGraph = hiddenGraph
      , graphBehavior = Selectable
      , simulation = (Force.simulation forces)
      , message =  "No message yet"}
      , Cmd.none )


setupGraph : Graph.Graph Network.NodeState ()
             -> ( List (Force.Force Int), Graph.Graph Network.Entity () )
setupGraph inputGraph =
    let
            outputGraph =
                Graph.mapContexts Network.initializeNode inputGraph

            link { from, to } =
                ( from, to )

            forces = 
                [ Force.links <| List.map link <| Graph.edges outputGraph
                , Force.manyBody <| List.map .id <| Graph.nodes outputGraph
                , Force.center (500 / 2) (500 / 2)
                ]
     in
       (forces, outputGraph)

computeForces : Graph.Graph n e -> List (Force.Force Int)
computeForces graph =
    let
        k = 2.0
        link { from, to } =
                    ( from,  to )
    in
        [  Force.customLinks 1 <| List.map alterLink <| List.map link <| Graph.edges graph
         , Force.manyBodyStrength -1.8 (List.map .id <| Graph.nodes graph)
         ,  Force.center (500 / 2) (500 / 2)
        ]


alterLink (from, to) =
    { source = from
    , target = to
    , distance = 90
    , strength = Just 1
    }

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


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick t ->
            let
                ( newState, list ) =
                    Force.tick model.simulation <| List.map .label <| Graph.nodes model.graph
            in
                case model.drag of
                    Nothing ->
                        { model | graph = updateGraphWithList model.graph list,simulation = newState}

                    Just { current, index } ->
                        { model | graph = (Graph.update index (Maybe.map (updateNode current)) (updateGraphWithList model.graph list) )}


        DragStart index xy ->
            { model | drag = Just (Drag xy xy index) }


        MouseClick index xy ->
            let
                associatedIncomingNodeIds = (Network.inComingNodeIds index model.hiddenGraph)
                associatedOutgoingNodeIds =  (Network.outGoingNodeIds index model.hiddenGraph)
                newGraph =
                      Network.setStatus  index Recruited  model.graph
                         |> Network.connect model.recruiter  index
                         |> Network.connectNodeToNodeInList model.recruiter associatedOutgoingNodeIds
                forces = computeForces newGraph

            in
                    { model | message = "Clicked node " ++ String.fromInt index
                              , graph = newGraph
                              , simulation = (Force.simulation forces)
                              , clickCount = model.clickCount + 1
                            }



        DragAt xy ->
            case model.drag of
                Just { start, index } ->
                    {  model | drag = Just (Drag start xy index)
                     , graph = Graph.update index (Maybe.map (updateNode xy)) model.graph
                     , simulation = Force.reheat model.simulation  }
                Nothing ->
                    model

        DragEnd xy ->
            case model.drag of
                Just { start, index } ->
                    {model | drag = Nothing, graph = Graph.update index (Maybe.map (updateNode xy)) model.graph}

                Nothing ->
                    model

        SetGraphBehavior behavior ->
             { model | graphBehavior = behavior }

        StartOver ->
            let
                (forces, graph) = setupGraph Network.testGraph
            in
            { model |   graph = graph
                      , simulation = (Force.simulation forces)
                      , clickCount = 0}

        ReHeat ->
                    { model |  simulation = Force.reheat model.simulation }


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
    -- Mouse.onDown (.clientPos >> MouseClick index)

onMouseClick : NodeId -> Attribute Msg
onMouseClick index =
    Mouse.onClick (.clientPos >> MouseClick index)

linkElement : Graph (Force.Entity Int { value : NodeState }) e
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
        mouseHandler = case model.graphBehavior of
            Selectable -> onMouseClick
            Draggable -> onMouseDown

    in
    g []
        [ circle
            [ r 14.0
            , if node.label.value.status == NotRecruited then  fill (Fill (Color.rgba 0.3 0 1 1.0)) else  fill (Fill (Color.rgba 1.0 0 0.3 1.0))
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
    column [spacing 12 , centerX, centerY] [
       row [ spacing 24 ] [
           leftPanel model
         , rightPanel model ]
      ]

leftPanel : Model -> Element Msg
leftPanel model =
    column [spacing 12, width (px 500), padding 40] [
       infoPanel model
       , controlPanel model
    ]

infoPanel : Model -> Element Msg
infoPanel model =
     column [spacing 12, width (px 450), padding 40, Border.width 1] [
            el [alignTop] (text "SIMULATION")
           , el [] (text "EXPERIMENTAL WORK IN PROGRESS")
           ,  el [Font.size 14] (text "Click on nodes to 'recruit' them.")
           ,  el [Font.size 14] (text "Clicking certain nodes will automatically recruit others.")
           ,  el [Font.size 14] (text "Why?")
          ]

controlPanel : Model -> Element Msg
controlPanel model =
    column [spacing 12, width (px 450), padding 40, Border.width 1, Font.size 16] [
               scoreIndicator model
               , row [spacing 12] [
                  el [] (text <| "Nodes: " ++ (String.fromInt <| Graph.size model.graph))
                 , el [] (text <| "Recruited nodes: " ++ (String.fromInt  (List.length <| recruitedNodes model)))
                 , el [] (text <| "Clicks: " ++ String.fromInt model.clickCount)
                 ]
             -- , row [spacing 12] [ enableSelectionButton model, enableDragginButton model]
             , row [spacing 12] [startOverButton model
                                 -- , reheatButton model
              ]
             ,el [] (text model.message)
              ]

scoreIndicator : Model -> Element Msg
scoreIndicator model =
    let
        cc = toFloat model.clickCount
        rn = toFloat <| List.length <| recruitedNodes model
        score = round <| (30*rn - 20*cc)
    in
        el [Font.size 24, Font.bold] (text <| "Score: " ++ String.fromInt score)


recruitedNodes : Model -> List NodeId
recruitedNodes model =
   Network.outGoingNodeIds model.recruiter model.graph


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
            |> List.map (nodeElement model)
            |> g [ class [ "nodes" ] ]
        ]



--
-- BUTTONS
--

startOverButton : Model -> Element Msg
startOverButton model =
    Input.button  (buttonStyle [ Background.color charcoal]) {
        onPress = Just (StartOver)
      , label = el [] (text  "Start Over")
    }

reheatButton : Model -> Element Msg
reheatButton model =
    Input.button  (buttonStyle [ Background.color charcoal]) {
        onPress = Just (ReHeat)
      , label = el [] (text  "Arrange")
    }

enableSelectionButton : Model -> Element Msg
enableSelectionButton model =
    Input.button  (buttonStyle [selectedBackground (model.graphBehavior == Selectable)]) {
        onPress = Just (SetGraphBehavior Selectable)
      , label = el [] (text  "Select Nodes")
    }

enableDragginButton : Model -> Element Msg
enableDragginButton model =
    Input.button  (buttonStyle [selectedBackground (model.graphBehavior == Draggable)]) {
        onPress = Just (SetGraphBehavior Draggable)
      , label = el [] (text "Move Nodes")
    }

buttonStyle extras =
    [Border.width 1, padding 8, Border.rounded 4, Font.color white] ++ extras

white = rgb 1 1 1
charcoal = rgb 0.3 0.3 0.3
darkRed = rgb 0.6 0 0


selectedBackground flag =
    case flag of
        True -> Background.color darkRed
        False -> Background.color charcoal


{- {"delay": 5001} -}
