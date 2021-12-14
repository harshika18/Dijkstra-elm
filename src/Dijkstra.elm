module Dijkstra exposing (..)


import Browser
import Dagre.Attributes as DA
import Graph as G
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import AnalyticsPort exposing (analytics)
import Core
import Core.Prompt as P
import Render as R
import Random as RD
import Random.List as RL
import Render.StandardDrawers as RSD
import Render.StandardDrawers.Attributes as RSDA
import Render.StandardDrawers.Types as RSDT
import String exposing (..)
import Array exposing (Array)
import List.Extra as LE
import Svg.Attributes as SA
import Parser exposing (number)
import Dict exposing (Dict)
import IntDict exposing (IntDict)
import Graph.DOT as GD
import Color as C
-- import Css.Colors as C
-- import Colors exposing (black)

simpleGraph : G.Graph Data ()
simpleGraph =
    let
        nodes =
            -- [0,1,2,3,4,5,6,7,8]
            [ G.Node 0 { id="0", label="51", status=Possible }
            , G.Node 1 { id="1", label="42", status=Possible }
            , G.Node 2 { id="2", label="31", status=Possible }
            , G.Node 3 { id="3", label="55", status=Possible }
            , G.Node 4 { id="4", label="44", status=Possible }
            , G.Node 5 { id="5", label="89", status=Possible }
            , G.Node 6 { id="6", label="62", status=Possible }
            , G.Node 7 { id="7", label="65", status=Possible }
            , G.Node 8 { id="8", label="71", status=Possible }
            ]

        e from to =
            G.Edge from to ()

        edges =
            [ ( e 0 1 )
            , ( e 0 2 )
            , ( e 1 3 )
            , ( e 1 4 )
            , ( e 2 5 )
            , ( e 2 6 )
            , ( e 3 7 )
            , ( e 3 8 )
            , ( e 0 7 )
            , ( e 0 5 )
            , ( e 4 0 )
            , ( e 6 0 )
            , ( e 4 8 )
            , ( e 6 8 )
            , ( e 7 8 )
            ]
        -- nodeList = List.map (\n -> G.Node n (String.fromInt n)) nodes
        -- edgeList = List.map (\(n1,n2) -> G.Edge n1 n2 ()) edges
  in
    G.fromNodesAndEdges nodes edges
    -- G.fromNodesAndEdges nodeList edges
    -- let
    --     nodeList = List.map (\n -> G.Node n (String.fromInt n)) model.nodes
    --     edgeList = List.map (\(n1,n2) -> G.Edge n1 n2 ()) model.edges
       
    
    -- in
    --     G.fromNodesAndEdges nodeList edgeList


type Msg
    = SelectNode (Int, List Int)
    | VisMin (G.Graph Data ())
    | InsertDist String
    | Submit
    | Got (List Int, List (Int,Int))

type alias Model =
    { queue : List Int
    , prompt : P.Prompt
    , minDist : List Int
    , visited : List Int
    , active : Int
    , selectednode : Int
    , inputdist : String
    , incoming : List Int
    , nodes : List Int 
    , edges : List (Int,Int)
    , defnode : G.NodeContext Data ()
    }


init : () -> (Model,Cmd Msg) 
init _ =
    ({ queue = [0]
    , prompt = ( """Click on Next Min button to start the algorithm. 0 is the source node""", P.PromptInfo )
    , minDist = [0, 100, 100, 100, 100, 100, 100, 100, 100]
    , visited = [1, 0, 0, 0, 0, 0, 0, 0, 0]
    , nodes = [1,2,3,4,5]
    , edges = []
    , selectednode = -1
    , inputdist = ""
    , incoming = []
    , active = -1
    , defnode = defNodeContext
    }, RD.generate Got (generateGraph 10 20))
    
updatequeue : List Int -> List Int
updatequeue q =     
    LE.unique q

selectnode : Int -> List Int -> Model -> Model
selectnode v income model =
    if List.member v model.queue 
    then 
    { model 
    | prompt = ( "You selected node " ++ String.fromInt v ++ ". Please enter the distance in the text box!", P.PromptInfo )
    , selectednode = v
    -- , defnode = {model.defnode |} 
    , incoming = income
    , inputdist = ""
    }
    else 
    { model
    | inputdist = ""
    , selectednode = -1
    , prompt = ("Can not update the distance of this node!", P.PromptInfo )
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        { queue, prompt, inputdist} =
            model
    in
    case msg of
        SelectNode (v,income) ->
            (selectnode v income model, Cmd.none)  
        VisMin g ->
            (visMin g model,Cmd.none )
        InsertDist d ->
            ({ model | inputdist = d },Cmd.none)
        Submit ->
            (submit model,Cmd.none)
        Got (m,n) ->
            ({model | nodes = m, edges = n},Cmd.none)
        
findMinFromQueue : List Int -> List Int -> Int -> Int -> Int
findMinFromQueue dist queue id min =
    let
        idx = Maybe.withDefault -1 (List.head queue)
    in
    if idx == -1
    then
        id
    else if (Maybe.withDefault 100 (get idx dist)) < min 
    then
        findMinFromQueue dist (List.drop 1 queue) idx (Maybe.withDefault -1 (get idx dist)) 
    else
        findMinFromQueue dist (List.drop 1 queue) id min

checkVisited : List Int -> List Int -> List Int -> List Int
checkVisited vis que ans =
    let
        idx = Maybe.withDefault -1 (List.head que)
    in
    if idx == -1
    then 
        ans
    else if (Maybe.withDefault -1 (get idx vis)) == 0
    then 
        checkVisited vis (List.drop 1 que) (ans ++ [idx]) 
    else
        checkVisited vis (List.drop 1 que) ans 

checkAllUpdated : List Int -> Int -> List Int -> Bool
checkAllUpdated outgoing active distance =
    let
         idx = Maybe.withDefault -1 (List.head outgoing)
         distFromActive = Maybe.withDefault -1 (get active distance) + 1
         originalDist = Maybe.withDefault -1 (get idx distance)
    in
    if idx == -1
    then 
        True
    else if distFromActive >= originalDist
    then 
        checkAllUpdated (List.drop 1 outgoing) active distance
    else 
        False 

visMin : G.Graph Data () -> Model -> Model
visMin g model =
    let
        actualminid = findMinFromQueue model.minDist model.queue -1 100
        outgoing = (G.alongOutgoingEdges (Maybe.withDefault (defNodeContext) (G.get actualminid g)))
        outgoingcheck = (G.alongOutgoingEdges (Maybe.withDefault (defNodeContext) (G.get model.active g)))
    in
    if checkAllUpdated outgoingcheck model.active model.minDist == False
    then 
    { model
    | prompt = ("Update not completed, check discovered nodes to update the distance." , P.PromptInfo)
    , selectednode = -1
    , inputdist = ""
    }
    else if actualminid == -1    
    then
    { model
    | active = actualminid
    , prompt = ("Queue is empty. Algorithm terminate here!!!" , P.PromptInfo)
    }
    else
    { model
    | active = actualminid
    , queue = LE.remove actualminid (checkVisited model.visited (updatequeue (model.queue ++ outgoing)) [])
    , visited = updateIndex model.visited actualminid 1
    , prompt = ("New Active Node is " ++ String.fromInt actualminid , P.PromptInfo)
    , inputdist = ""
    , selectednode = -1
    }

get nth list = list |> List.drop (nth) |> List.head

submit : Model -> Model
submit model = 
    if model.selectednode == -1
    then
    { model 
    | prompt = ( "Please select a Node to proceed.", P.PromptInfo  )
    , inputdist = ""
    , selectednode = -1
    }
    else
        checkMin model

updateIndex : List Int -> Int -> Int -> List Int
updateIndex a idx replace =
    (List.take idx a) ++ [replace] ++ (List.drop (idx + 1) a)

checkMin : Model -> Model
checkMin model =
    let
        originalDist = Maybe.withDefault -1 (get model.selectednode model.minDist)
        actualmin = Maybe.withDefault -1 (List.minimum (incomingDistArray model.minDist model.incoming [Maybe.withDefault -1 (get model.selectednode model.minDist)]))
        displaydistint = Maybe.withDefault 100 (String.toInt model.inputdist)
    in
    if actualmin == displaydistint
    then
    { model
    | inputdist = ""
    , selectednode = -1
    , prompt = ("Correct distance!!!", P.PromptInfo )
    , minDist = updateIndex model.minDist model.selectednode displaydistint
    }
    else
    { model
    | prompt = ("Incorrect distance!!" , P.PromptInfo )
    , inputdist = ""
    , selectednode =-1
    }

defNodeContext : G.NodeContext Data ()
defNodeContext = 
    {node={id=-1,label={id="-1",status=Idle,label="DEF NODE"}},incoming=IntDict.empty,outgoing=IntDict.empty}


incomingDistArray : List Int -> List Int -> List Int -> List Int
incomingDistArray dist incoming ans =
    let
        idx = Maybe.withDefault -1 (List.head incoming)
    in
    if idx == -1
    then
        ans
    else
        incomingDistArray dist (List.drop 1 incoming) (ans ++  [(Maybe.withDefault -1 (get idx dist)) + 1])

type alias Data =
    { id : String
    ,  label : String
    , status : Status}

type Status 
    = Active
    | Idle
    | Possible
    | Visited


viewGraph : G.Graph Data () -> Model -> Html.Html Msg
viewGraph g model=
    R.draw
        [ DA.rankDir DA.LR
        ]
        -- []
        [ R.nodeDrawer
            (RSD.svgDrawNode
                [ RSDA.onClick (\n -> SelectNode ( n.id, (G.alongIncomingEdges (Maybe.withDefault (defNodeContext) (G.get n.id g)))))
                -- [ RSDA.onClick (\n -> )
                , RSDA.strokeColor (strokeColor model.selectednode)
                , RSDA.strokeWidth (\_ -> 3)
                ]
            )
        , R.edgeDrawer
            (RSD.svgDrawEdge
                [ RSDA.arrowHead RSDT.Vee
                , RSDA.strokeWidth (\_ -> 4)
                ]
            )
        , R.style "height: 80vh;"
        ]
        g

strokeColor : Int -> G.Node n -> C.Color
strokeColor selectednode node =
    if (Maybe.withDefault -1 (Just node.id)) == selectednode then
        C.red
    else 
        C.darkBlue


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ HA.type_ t, HA.placeholder p, HA.value v, HE.onInput toMsg ] []


view : Model -> Html.Html Msg
view model =
    let
        { queue, prompt, minDist, visited} =
            model
        disp_queue = concat ["Queue : {",(join " " (List.map String.fromInt (queue))),"}"]
        disp_dist = concat ["Minimum Distance : {",(join " " (List.map String.fromInt (minDist))),"}"]
        disp_vis = concat ["Visited : {",(join " " (List.map String.fromInt (visited))),"}"]
        disp_enter_dist = concat ["Active Node : ", String.fromInt model.active]
    in
    Html.div
        [ HA.class "experiment-container" ]
        [ Html.div
            [ HA.class "feedback-container"
            ]
            [ P.show prompt
            ]
        , Html.div
            [ HA.class "observables-container" ]
            [ viewGraph simpleGraph model
            ]
        , Html.div
            [ HA.class "controls-container"
            , HA.style "padding-bottom" "20px"
            , HA.style "padding-left" "20px" ]
            [ Html.button [ HE.onClick (VisMin simpleGraph), HA.class "button__action--primary" ] [ Html.text "Visit Min" ]
            ] 
        , Html.div []
            [ viewInput "text" "Enter Distance" model.inputdist InsertDist
            , Html.button [ HE.onClick Submit, HA.class "button__action--primary" ] [ Html.text "Submit" ]
            ]
        , Html.div
            [ HA.style "font-size" "25px"]
            [ Html.text disp_queue ]
        , Html.div
            [ HA.style "font-size" "25px"]
            [ Html.text disp_dist ]
        , Html.div
            [ HA.style "font-size" "25px"]
            [ Html.text disp_vis ]
        , Html.div
            [ HA.style "font-size" "25px"]
            [ Html.text disp_enter_dist ]
        ]

-- main : Program () Model Msg
-- main =
--     Browser.sandbox
--         { init = init
--         , view = view
--         , update = update
--         }

main =
    Browser.element
        { init = Core.init identity analytics init
        , view = Core.view view
        , update = Core.update identity analytics update setFresh Nothing Nothing
        , subscriptions = Core.subscriptions subscriptions
        }

setFresh msg =
    case msg of
        -- Init _ ->
        --     True

        _ ->
            False


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

generateGraph : Int -> Int -> RD.Generator (List Int, List (Int,Int))
generateGraph nodesCnt edgesCnt =
    RD.list nodesCnt (RD.int 0 (2*nodesCnt))
    |> RD.andThen (\l -> RD.pair (RD.constant l) 
                                (generateEdge edgesCnt ([],l) [])
                 )


generateEdge : Int -> (List Int,List Int) -> List (Int,Int) -> RD.Generator (List (Int,Int))
generateEdge eCnt (selNodes,unSelNodes) edges =
    if eCnt == 0 then
        RD.constant edges
    else if List.length selNodes == 0  && List.length edges == 0 then
        RL.choices 2 unSelNodes
        |> RD.andThen (\(l1,l2) -> case l1 of 
                                    [] ->
                                        generateEdge eCnt (selNodes,unSelNodes) edges
                                    [n] -> 
                                        generateEdge eCnt (List.append selNodes l1,l2) edges
                                    [n1,n2] ->
                                        generateEdge (eCnt-1) (List.append selNodes l1, l2) (List.append edges [(n1,n2)])
                                    n :: ns ->
                                        let
                                            newEdges = List.map (\n2 -> (n,n2)) ns 
                                        in
                                        generateEdge (eCnt - List.length newEdges) (List.append selNodes l1, l2) (List.append edges newEdges)
                    )
    else
        RL.choose selNodes
        |> RD.andThen (\(n,l) -> case n of
                                    Just n1 ->
                                        if List.length unSelNodes == 0 then
                                            RL.choose selNodes 
                                            |> RD.andThen (\(n2_,l2) ->
                                                case n2_ of
                                                    Just n2 ->
                                                       generateEdge (eCnt-1) (selNodes,unSelNodes) ((n1,n2):: edges)
                                                    Nothing ->
                                                       generateEdge eCnt (selNodes,unSelNodes) edges 
                                               )
                                        else
                                            RL.choose unSelNodes 
                                            |> RD.andThen (\(n2_,l2) ->
                                                case n2_ of
                                                    Just n2 ->
                                                       generateEdge (eCnt-1) (n2::selNodes ,l2) ( (n1,n2) :: edges )
                                                    Nothing ->
                                                       generateEdge eCnt (selNodes,unSelNodes) edges
                                               )
                                    Nothing ->
                                       generateEdge eCnt (selNodes,unSelNodes) edges
                     )                   
