port module Ports exposing
    ( get
    , put
    , quit
    )


port get : (String -> msg) -> Sub msg


port put : String -> Cmd msg


port quit : String -> Cmd msg
