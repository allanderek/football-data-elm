port module Ports exposing
    ( get
    , put
    , quit
    , resize
    )

import Json.Decode exposing (Value)


port resize : (Value -> msg) -> Sub msg


port get : (String -> msg) -> Sub msg


port put : String -> Cmd msg


port quit : String -> Cmd msg
