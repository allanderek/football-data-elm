port module Ports exposing
    ( get
    , put
    )


port get : (String -> msg) -> Sub msg


port put : String -> Cmd msg
