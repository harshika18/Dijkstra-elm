module Dijkstra exposing (..)


import Browser
import Dagre.Attributes as DA
import Graph as G
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import Core
import Core.Prompt as P
import Render as R
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


simpleGraph : G.Graph Data ()
simpleGraph =
    -- G.fromNodeLabelsAndEdgePairs
    --     [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
    --     [ ( 0, 1 )
    --     , ( 0, 2 )
    --     , ( 1, 3 )
    --     , ( 1, 4 )
    --     , ( 2, 5 )
    --     , ( 2, 6 )
    --     , ( 3, 7 )
    --     , ( 3, 8 )
    --     , ( 0, 7 )
    --     , ( 0, 5 )
    --     , ( 4, 0 )
    --     , ( 6, 0 )
    --     , ( 4, 8 )
    --     , ( 6, 8 )
    --     , ( 7, 8 )
    --     ]
    let
        nodes =
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
            -- [ e 0 3  
            -- , e 1 2  
            -- , e 1 3  
            -- , e 2 3  
            -- , e 2 6  
            -- , e 5 6  
            -- , e 5 7  
            -- , e 6 8  
            -- , e 7 8  
            -- ]
  in
    G.fromNodesAndEdges nodes edges



type Msg
    = SelectNode (Int, List Int)
    | VisMin (G.Graph Data ())
    | InsertDist String
    | Submit

type alias Model =
    { queue : List Int
    , prompt : P.Prompt
    , minDist : List Int
    , visited : List Int
    , parent : Int
    , selectednode : Int
    , inputdist : String
    -- , displaydist : String
    , incoming : List Int
    }


init : Model
init =
    { queue = [0]
    , prompt = ( """Click on Next Min button to start the algorithm""", P.PromptInfo )
    , minDist = [0, 100, 100, 100, 100, 100, 100, 100, 100]
    , visited = [1, -1, -1, -1, -1, -1, -1, -1, -1]
    , selectednode = -1
    , inputdist = ""
    -- , displaydist = ""
    , incoming = []
    , parent = 0
    }
    
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
    , incoming = income
    }
    else 
    { model
    -- | displaydist = ""
    | inputdist = ""
    , selectednode = -1
    , prompt = ("Can not update the distance of this node!", P.PromptInfo )
    }

update : Msg -> Model -> Model
update msg model =
    let
        { queue, prompt, inputdist} =
            model
    in
    case msg of
        SelectNode (v,income) ->
            selectnode v income model  
        VisMin g ->
            visMin g model 
        -- UpdateIncoming v ->
        --     { model
        --     | incoming = v
        --     }    
        InsertDist d ->
            { model | inputdist = d }
        Submit ->
            submit model
            -- { model 
            -- | displaydist = inputdist
            -- , inputdist = ""
            -- }      
        -- _ ->
        --     model

findMinFromQueue : List Int -> List Int -> Int -> Int -> Int
findMinFromQueue dist queue id min =
    let
        idx = Maybe.withDefault -1 (List.head queue)
        -- dis = Array.fromList dist
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
        -- dis = Array.fromList dist
    in
    if idx == -1
    then 
        ans
    else if (Maybe.withDefault -1 (get idx vis)) == -1
    then 
        checkVisited vis (List.drop 1 que) (ans ++ [idx]) 
    else
        checkVisited vis (List.drop 1 que) ans 

visMin : G.Graph Data () -> Model -> Model
visMin g model =
    let
        -- outgoing = (G.alongOutgoingEdges (Maybe.withDefault (defNodeContext n) (G.get n.id g)))
        -- tempqueue = LE.remove model.parent model.queue
        actualminid = findMinFromQueue model.minDist model.queue -1 100
        -- if actualminid == -1
        -- then
        --     outgoing = []
        --     tempqueue = updatequeue (model.queue)
        --     updatedqueue = checkVisited model.visited tempqueue []
        -- else 
        outgoing = (G.alongOutgoingEdges (Maybe.withDefault (defNodeContext) (G.get actualminid g)))
            -- tempqueue = updatequeue (model.queue ++ outgoing)
            -- updatedqueue = checkVisited model.visited tempqueue []
    in
    if actualminid == -1    
    then
    { model
    | parent = actualminid
    -- , queue = checkVisited model.visited model.queue []
    -- , visited = updateIndex model.visited actualminid 1
    , prompt = ("Queue is empty" , P.PromptInfo)
    }
    else
    { model
    | parent = actualminid
    , queue = LE.remove actualminid (checkVisited model.visited (updatequeue (model.queue ++ outgoing)) [])
    , visited = updateIndex model.visited actualminid 1
    , prompt = ("New Parent is " ++ String.fromInt actualminid , P.PromptInfo)
    }

get nth list = list |> List.drop (nth) |> List.head

submit : Model -> Model
submit model = 
    if model.selectednode == -1
    then
    { model 
    | prompt = ( "Select a Node to proceed.", P.PromptInfo  )
    -- , displaydist = ""
    , inputdist = ""
    }
    else
        checkMin model

updateIndex : List Int -> Int -> Int -> List Int
updateIndex a idx replace =
    (List.take idx a) ++ [replace] ++ (List.drop (idx + 1) a)

-- checkMin : Model -> Model
-- checkMin model =
--     let
--         distFromParent = (Maybe.withDefault -1 (get model.parent model.minDist)) + 1
--         originalDist = Maybe.withDefault -1 (get model.selectednode model.minDist)
--         -- incomingnodes = model.incoming
--         -- actualmin = Maybe.withDefault -1 (List.minimum (incomingDistArray model.minDist incomingnodes [Maybe.withDefault -1 (get (model.selectednode + 1) model.minDist)]))
--         displaydistint = Maybe.withDefault 100 (String.toInt model.inputdist)
--     in
--     if (distFromParent == displaydistint && distFromParent < originalDist) || (originalDist == displaydistint && distFromParent >= originalDist)
--     then
--     { model
--     | displaydist = model.inputdist
--     , inputdist = ""
--     , prompt = ("Correct distance!!!" ++ String.fromInt displaydistint, P.PromptInfo )
--     , minDist = updateIndex model.minDist model.selectednode displaydistint
--     }
--     else
--     { model
--     | prompt = ("Incorrect distance!!" , P.PromptInfo )
--     , displaydist = ""
--     , inputdist = ""
--     }

checkMin : Model -> Model
checkMin model =
    let
        -- distFromParent = (Maybe.withDefault -1 (get model.parent model.minDist)) + 1
        originalDist = Maybe.withDefault -1 (get model.selectednode model.minDist)
        incomingnodes = model.incoming
        actualmin = Maybe.withDefault -1 (List.minimum (incomingDistArray model.minDist incomingnodes [Maybe.withDefault -1 (get model.selectednode model.minDist)]))
        displaydistint = Maybe.withDefault 100 (String.toInt model.inputdist)
    in
    if actualmin == displaydistint
    then
    { model
    | inputdist = ""
    -- ,displaydist = model.inputdist
    , prompt = ("Correct distance!!!" ++ String.fromInt displaydistint, P.PromptInfo )
    , minDist = updateIndex model.minDist model.selectednode displaydistint
    }
    else
    { model
    | prompt = ("Incorrect distance!!" , P.PromptInfo )
    -- , displaydist = ""
    , inputdist = ""
    }


-- odel 
            -- | displaydist = inputdist
            -- , inputdist = ""
            -- }      
        -- _ ->
-- defNodeContext : NodeContext Data ()
defNodeContext = 
    {node={id=-1,label={id="-1",status=Idle,label="DEF NODE"}},incoming=IntDict.empty,outgoing=IntDict.empty}


incomingDistArray : List Int -> List Int -> List Int -> List Int
incomingDistArray dist incoming ans =
    let
        idx = Maybe.withDefault -1 (List.head incoming)
        -- dis = Array.fromList dist
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


viewGraph : G.Graph Data () -> Html.Html Msg
viewGraph g =
    R.draw
        [ DA.rankDir DA.LR
        ]
        -- []
        [ R.nodeDrawer
            (RSD.svgDrawNode
                [ RSDA.onClick (\n -> SelectNode ( n.id, (G.alongIncomingEdges (Maybe.withDefault (defNodeContext) (G.get n.id g)))))
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

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ HA.type_ t, HA.placeholder p, HA.value v, HE.onInput toMsg ] []


view : Model -> Html.Html Msg
view model =
    let
        { queue, prompt, minDist, visited} =
            model
        disp_queue = concat ["Queue : {",(join " " (List.map String.fromInt (queue))),"}"]
        disp_dist = concat ["Minimum Dist : {",(join " " (List.map String.fromInt (minDist))),"}"]
        disp_vis = concat ["Visited : {",(join " " (List.map String.fromInt (visited))),"}"]
        disp_enter_dist = concat ["Parent : ", String.fromInt model.parent]
    in
    Html.div
        [ HA.class "experiment-container" ]
        [ Html.div
            [ HA.class "feedback-container" ]
            [ P.show prompt
            ]
        , Html.div
            [ HA.class "observables-container" ]
            [ viewGraph simpleGraph
            ]
        , Html.div
            [ HA.class "controls-container"
            , HA.style "padding-bottom" "20px"
            , HA.style "padding-left" "20px" ]
            [ Html.button [ HE.onClick (VisMin simpleGraph), HA.class "button__action--primary" ] [ Html.text "Next Min" ]
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

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }