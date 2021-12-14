port module AnalyticsPort exposing (analytics)

import Json.Encode as JE

port analytics : JE.Value -> Cmd msg