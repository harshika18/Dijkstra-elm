module Dijkstra exposing (..)


import Browser
import Dagre.Attributes as DA
import Graph as G
import Html exposing (Html)
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

-- type alias Model =
--     String


simpleGraph : G.Graph Int ()
simpleGraph =
    G.fromNodeLabelsAndEdgePairs
        [ 0, 1, 2, 3, 4, 5, 6, 7, 8 ]
        [ ( 0, 1 )
        , ( 0, 2 )
        , ( 1, 3 )
        , ( 1, 4 )
        , ( 2, 5 )
        , ( 2, 6 )
        , ( 3, 7 )
        , ( 3, 8 )
        , ( 0, 7 )
        , ( 0, 5 )
        , ( 4, 0 )
        , ( 6, 0 )
        , ( 4, 8 )
        , ( 6, 8 )
        , ( 7, 8 )
        ]


type Msg
    = SelectNode (List Int)
    | VisMin

type alias Model =
    { queue : List Int
    , prompt : P.Prompt
    , minDist : List Int
    , parent : List Int
    , selectednode : Int
    }


init : Model
init =
    { queue = []
    , prompt = ( """No element selected!!, click on an edge/node to select it.""", P.PromptInfo )
    , minDist = [0, 100, 100, 100, 100, 100, 100, 100, 100]
    , parent = [-1, -1, -1, -1, -1, -1, -1, -1, -1]
    , selectednode = -1
    }
    

update : Msg -> Model -> Model
update msg model =
    let
        { queue, prompt} =
            model
    in
    case msg of
        SelectNode v ->
            { model 
            -- | prompt = ( "You selected node " ++ String.fromInt v, P.PromptInfo )
            | queue = queue ++ v
            }            
        SelectEdge ( from, to ) ->
            { model
            | prompt = ( "You selected edge from " ++ String.fromInt from ++ " to " ++ String.fromInt to , P.PromptInfo )
            }

defNodeContext n = 
    { node=n
    , incoming=IntDict.empty
    , outgoing=IntDict.empty
    }



viewGraph : G.Graph n e -> Html.Html Msg
viewGraph g =
    R.draw
        [ DA.rankDir DA.LR
        ]
        -- []
        [ R.nodeDrawer
            (RSD.svgDrawNode
                -- [ RSDA.onClick (\n -> SelectNode n.id)
                -- [ RSDA.onClick (\n -> SelectNode (List.reverse (G.dfs (G.onDiscovery (::)) [] g).incoming))
                [ RSDA.onClick (\n -> SelectNode (G.alongOutgoingEdges (Maybe.withDefault (defNodeContext n) (G.get n.id g))))
                ]
            )
        , R.edgeDrawer
            (RSD.svgDrawEdge
                [ RSDA.arrowHead RSDT.Vee
                , RSDA.onClick (\e -> SelectEdge ( e.from, e.to ))
                , RSDA.strokeWidth (\_ -> 4)
                ]
            )
        , R.style "height: 80vh;"
        ]
        g

view : Model -> Html.Html Msg
view model =
    let
        { queue, prompt} =
            model
        disp_pivot = join " " (List.map String.fromInt (queue))
        disp_pivot1 = concat ["{",disp_pivot,"}"]

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
            [ HA.style "text-align" "center"
            , HA.style "padding-bottom" "40px"
            , HA.style "font-weight" "bold"
            , HA.style "font-family" "Ubuntu"
            , HA.style "font-size" "25px"]
            [ Html.text disp_pivot1 ] 
        ]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }