port module BFS_Forest_Span exposing (..)


import Browser exposing(..)
import Graph exposing (..)
import Graph.DOT as Dot exposing (..)
import Dict exposing (..)
import IntDict exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled, id)
import Svg.Attributes exposing (mode)
import Html.Attributes exposing (list)
import Html.Attributes
import Random


{- ##Flow
 [ Graph -> dotScript ] --elm to JS with port--> 
 [RecvDotStr -Viz.js-> SVG in HTML --Add onclick--> SVGwithClicks ]
 --JS to elm with ports--> [Msg -> Update -> Model]
 -}



-- Add a general style for the Graph elements
myStyles =
    { defaultStyles
        | node = "style=\"rounded, filled\", penwidth=0" 
        , edge = "dir=none, penwidth=2"
    }

-- A mapping function for Nodes to Node Label(used by Graph to DOT)
nodeLabel : Data -> Maybe String
nodeLabel n = 
  if n.status == Active then
      Just (n.label ++ " (Active) ")
  else
      Just (n.label ++ " (Idle) ")


-- A mapping function for Edge to Edge Label(used by Graph to DOT)
edgeLabel : e -> Maybe String
edgeLabel e = Just " "


-- A mapping function for Nodes to Node Attributes(used by Graph to DOT)
nodeLabel2 : Data -> Dict String String
nodeLabel2 n = 
    if n.status == Active then
        Dict.fromList [ ("label", n.label),("fillcolor","YELLOW"),("id",n.id),("penwidth","3")]
    else if n.status == Idle then
        Dict.fromList [ ("label", n.label),("fillcolor","#F5CBCB"),("id",n.id)]
    else if n.status == Possible then
        Dict.fromList [ ("label", n.label),("fillcolor","ORANGE"),("id",n.id),("class","selectableNode")]
    else
        Dict.fromList [ ("label", n.label),("fillcolor","GREEN"),("id",n.id)]

-- A mapping function for Edge to Edge Attributes(used by Graph to DOT)
-- Remove the ("dir","none") to show arrow direction
edgeLabel2 : () -> Dict String String
edgeLabel2 e = Dict.fromList [ ("label", " ")]


getDotStr : Graph Data () -> String
getDotStr g =
    Dot.outputWithStylesAndAttributes myStyles nodeLabel2 edgeLabel2 g


port toJs : String -> Cmd msg
port toJs2 : String -> Cmd msg
port fromJs1 : (Int -> msg) -> Sub msg
port fromJs2 : (Int -> msg) -> Sub msg

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        ,subscriptions = subscriptions
    }



type alias Model 
  = {
      graph : Graph Data ()
      ,dotStr : String
      , queue : List Int
      , visited : List Int
      , backtrack : Bool
      , repositionRoot : Bool
      , tree : Tree
  }


init : () -> (Model, Cmd Msg)
init _ = ({graph = dressUp, dotStr=getDotStr dressUp, queue=[],visited=[], backtrack=False,repositionRoot=False, tree={nodes=[],edges=[]}}, Random.generate NewGraph (randomNodesEdges))
type Msg
    = Msg1
    | SendToJs
    | Select Int
    | DeSelect Int
    | Backtrack
    | RepositionRoot
    | NewGraph (List Int, List Int)



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Msg1 ->
            ( { model | dotStr = Dot.outputWithStylesAndAttributes myStyles nodeLabel2 edgeLabel2 model.graph},Cmd.none)
        SendToJs ->
          (model, toJs model.dotStr)
        Select id ->
          let
              newVisited = id :: model.visited 
              newQueue = List.append model.queue [id]
              oldActive = (Maybe.withDefault -1 (List.head model.queue))
              newActive = (Maybe.withDefault -1 (List.head newQueue))
              graphUpdatedVisited = (Graph.update id (Maybe.map (updateStatusNodeCtx Visited)) model.graph)
              unExplored = (getUnexplored (getAllPossibleNodes graphUpdatedVisited) newVisited)
              graphUpdatedIdle = (updateNodesStatus Idle  unExplored graphUpdatedVisited) 
              graphUpdatedActive = (Graph.update newActive (Maybe.map (updateStatusNodeCtx Active)) graphUpdatedIdle)
              possibleUnExplored = (getUnexplored (getPossibleNodes newActive graphUpdatedActive) newVisited) 
              graphUpdatedPossible = (updateNodesStatus Possible possibleUnExplored graphUpdatedActive)
              newDotStr = (getDotStr graphUpdatedPossible)
              newTree = (addToTree id (oldActive,id) model.tree)
          in
          
          ( { model | queue = newQueue, visited = newVisited
          , graph = graphUpdatedPossible, dotStr = newDotStr
          , backtrack = List.isEmpty possibleUnExplored
          , tree = newTree
          }
            ,Cmd.batch [toJs newDotStr,toJs2 (getSpanTree newTree model.graph)]
          )
        DeSelect id ->
          let
              --newStack = id :: model.stack 
              newGraph = (Graph.update id (Maybe.map (updateStatusNodeCtx Idle)) model.graph)
              newDotStr = (getDotStr newGraph)
          in
          
          ( { model | graph = newGraph, dotStr = newDotStr}
            ,toJs newDotStr
          )
        RepositionRoot->
            let
              possibleUnExplored = (getUnexplored (Graph.nodeIds model.graph ) model.visited) 
              graphUpdatedPossible = (updateNodesStatus Possible possibleUnExplored model.graph)
              newDotStr = (getDotStr graphUpdatedPossible)
          in
          
          ( { model | graph = graphUpdatedPossible, dotStr = newDotStr
                      ,backtrack = False
                      , repositionRoot = (List.isEmpty possibleUnExplored) && (Graph.size model.graph /= List.length model.visited) }
            ,toJs newDotStr
          )
        Backtrack ->
            let
              oldActive = (Maybe.withDefault -1 (List.head model.queue))
              newQueue = (Maybe.withDefault [] (List.tail model.queue))
              id = (Maybe.withDefault -1 (List.head newQueue))
              graphUpdatedVisited = (Graph.update oldActive (Maybe.map (updateStatusNodeCtx Visited)) model.graph)
              graphUpdatedActive = (Graph.update id (Maybe.map (updateStatusNodeCtx Active)) graphUpdatedVisited)
              possibleUnExplored = (getUnexplored (getPossibleNodes id graphUpdatedActive) model.visited) 
              graphUpdatedPossible = (updateNodesStatus Possible possibleUnExplored graphUpdatedActive)
              newDotStr = (getDotStr graphUpdatedPossible)
          in
          
          ( { model | queue = newQueue,  graph = graphUpdatedPossible, dotStr = newDotStr
                      ,backtrack = (not (List.isEmpty newQueue)) && (List.isEmpty possibleUnExplored)
                      , repositionRoot = (List.isEmpty newQueue) && (List.isEmpty possibleUnExplored)  }
            ,toJs newDotStr
          )
        NewGraph (t1,t2) ->
            ( {model | graph= (getNewGraph t1 t2), dotStr = getDotStr (getNewGraph t1 t2) }
            , toJs (getDotStr (getNewGraph t1 t2))
            )
      
type alias Alert = 
    {
        backtrack : String
        , reposition : String
        , visit :  String
        , start : String
        , finish : String
    }
alert : Alert
alert = {
    backtrack="No More Adjacent Nodes, Click BACKTRACK to traverse the next level !"
    , reposition = "No More Visitable Nodes in the current Connected Component of Graph, Please Click REPOSITION ROOT and select New Root to traverse the Next Connected Component !"
    , visit = "Please Click the Adjacent Node you want to visit Next !"
    , start = "Please Select a Root to traverse the Connected Component !"
    , finish = "BFS Completed, Spanning Tree Generated"
    }

viewAlert : Model -> Html Msg
viewAlert model =
    if (List.isEmpty model.visited) then
        div [id "Alert", Html.Attributes.style "color" "red" ] [text alert.start]
    else if (model.backtrack) == True then
        div [id "Alert", Html.Attributes.style "color" "red" ] [text alert.backtrack]
    else if (model.repositionRoot) == True then
        div [id "Alert", Html.Attributes.style "color" "red" ] [text alert.reposition]
    else if (List.length model.visited) == (List.length (Graph.nodeIds model.graph)) then
        div [id "Alert", Html.Attributes.style "color" "green" ] [text alert.finish]
    else
        div [id "Alert", Html.Attributes.style "color" "red" ] [text alert.visit]




view : Model -> Html Msg
view model =
  div [] [
    button [onClick Backtrack, disabled (not model.backtrack)] [text "BACKTRACK"] 
    , button [onClick RepositionRoot, disabled (not model.repositionRoot)][text "REPOSITION ROOT"]
    , div [id "main"] [
    div [id "queue"] [div [] [text "Queue : "],
                div[](List.map(\el -> div[Html.Attributes.style  "border" "solid",Html.Attributes.style "width" "10el"][text el]) (getNodeLabels model.queue model.graph )) ]
    , div [id "visited"] [span [] [text "Visiting Order : "],
               span [] [ text (List.map (\s-> s ++ " , ") (getNodeLabels (List.reverse model.visited) model.graph) |> String.concat) ]]
    , viewAlert model
    ]
  ]

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch
      [ fromJs1 Select
      , fromJs2 DeSelect
      ]

type Status 
    = Active
    | Idle
    | Possible
    | Visited

type alias Data =
    { id : String
    ,  label : String
    , status : Status}

-- Taken from Elm Package Graph 6.0.0 Documentation example
-- node labels are strings, edge labels are empty
dressUp : Graph Data () 
dressUp =
  let
    nodes =
      [ Node 0 { id="0", label="51", status=Possible }
      , Node 1 { id="1", label="42", status=Possible }
      , Node 2 { id="2", label="31", status=Possible }
      , Node 3 { id="3", label="55", status=Possible }
      , Node 4 { id="4", label="44", status=Possible }
      , Node 5 { id="5", label="89", status=Possible }
      , Node 6 { id="6", label="62", status=Possible }
      , Node 7 { id="7", label="65", status=Possible }
      , Node 8 { id="8", label="71", status=Possible }
      ]

    e from to =
      Edge from to ()

    edges =
      [ e 0 3  
      , e 1 2  
      , e 1 3  
      , e 2 3  
      , e 2 6  
      , e 5 6  
      , e 5 7  
      , e 6 8  
      , e 7 8  
      ]
  in
    Graph.fromNodesAndEdges nodes edges





updateStatusNodeCtx : Status -> NodeContext Data () -> NodeContext Data ()
updateStatusNodeCtx s nCtx = 
  let
      node = nCtx.node
  in
      {nCtx | node = (updateStatus s node) }

updateStatus : Status -> Node Data -> Node Data
updateStatus s node =
  let
      label = node.label
      newlabel = {label | status = s}
  in
   { node | label = newlabel }

defNodeContext : NodeContext Data ()
defNodeContext = 
    {node={id=-1,label={id="-1",status=Idle,label="DEF NODE"}},incoming=IntDict.empty,outgoing=IntDict.empty}


-- To getOnlyOutward Possible nodes remove the list append and removing Incoming edges
getPossibleNodes : NodeId -> Graph Data () -> List NodeId 
getPossibleNodes id g = 
    List.append (alongOutgoingEdges (Maybe.withDefault defNodeContext (Graph.get id g)))
                (alongIncomingEdges (Maybe.withDefault defNodeContext (Graph.get id g)))

updateNodesStatus : Status -> List NodeId -> Graph Data () -> Graph Data ()
updateNodesStatus s lst g =
    case lst of
        x :: xs ->
            (updateNodesStatus s xs (Graph.update x (Maybe.map (updateStatusNodeCtx s)) g))
        [] ->
            g

getUnexplored : List Int -> List Int -> List Int
getUnexplored lst visited =
    case lst of
        x :: xs ->
            if List.member x visited then
                (getUnexplored xs visited)
            else
                x :: (getUnexplored xs visited)
        []  ->
            []

checkStatus : Status -> NodeContext Data () ->Bool
checkStatus s nodeCtx=
    if nodeCtx.node.label.status == s then 
        True
    else
        False 

getAllPossibleNodes : Graph Data () -> List NodeId
getAllPossibleNodes g =
    let
        nodeList = Graph.nodeIds g
        nodeCtxList = List.map(\n->(Maybe.withDefault defNodeContext (Graph.get n g))) nodeList
    in
        List.map(\n-> n.node.id) (List.filter (checkStatus Possible) nodeCtxList)
    

getNodeLabels : List Int -> Graph Data () -> List String
getNodeLabels l g =
    let
        nodeCtxList = List.map(\n->(Maybe.withDefault defNodeContext (Graph.get n g))) l
    in
        List.map(\n -> n.node.label.label) nodeCtxList    

type alias Tree =
    {
        nodes : List Int
        , edges : List (Int, Int)
    }

listToTuple : List String -> (String,String)
listToTuple lst=
    case lst of 
        x1::x2::xs ->
            (x1,x2)
        _ ->
            ("None","None")

addToTree : Int -> (Int,Int) -> Tree -> Tree
addToTree n e t =
    if Tuple.first e == -1 then
        let
            nodes = List.append t.nodes [n]
        in
            {t | nodes = nodes }
    else
        let
            nodes = List.append t.nodes [n]
            edges = List.append t.edges [ e ]
        in
            {t | nodes = nodes, edges=edges}

posNode : List Int -> Int -> Int 
posNode lst id =
    case lst of 
        x :: xs ->
            if x == id then 0
            else 1 + (posNode xs id )
        [] ->
            -999

edgeFromNodes: List (Int,Int) -> List (Int) -> List (Int,Int)
edgeFromNodes el nl =
        List.map (\e-> (Tuple.mapBoth (posNode nl) (posNode nl) e) ) el
    

getSpanTree : Tree -> Graph Data () -> String
getSpanTree t g =
    let
        edges = edgeFromNodes t.edges t.nodes
        nodes = getNodeLabels t.nodes g
        graph = fromNodeLabelsAndEdgePairs nodes edges
    in
        Dot.output (\n->Just n) (\e-> Just " ") graph

randomNodesEdges : Random.Generator (List Int,List Int)
randomNodesEdges =
    Random.int 5 10
      |> Random.andThen (\len -> Random.pair (Random.list len (Random.int 1 50)) (Random.list ((len*(len-1))//2) (Random.int 1 50)) ) 
getNumbersTill : Int -> List Int
getNumbersTill n =
    if n > 0 then
        n :: (getNumbersTill (n-1))
    else    
        []
allEdgeList : Int -> List (Int,Int)
allEdgeList l =
    if l > 1 then
        List.append (List.map (\n -> Tuple.pair (l-1) (n-1)) (getNumbersTill (l-1))) (allEdgeList (l-1))
    else    
        []

returnEdge : Int -> (Int,Int) -> (Int,Int)
returnEdge p e =
    if modBy 2 p==0 then    e
    else        (-1,-1)


returnNode : Int -> Int -> Data
returnNode id label =
    {id= String.fromInt (id-1), label=String.fromInt label,status = Possible}
getNewGraph : List Int -> List Int -> Graph Data ()
getNewGraph nodeLst edgeLst =
    let
        nodes = List.map2 (returnNode) (List.reverse (getNumbersTill (List.length nodeLst))) nodeLst
        edges=List.map2 (returnEdge)  edgeLst (allEdgeList (List.length nodeLst))
    in
        fromNodeLabelsAndEdgePairs nodes edges
    