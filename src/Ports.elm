port module Ports exposing
    ( get
    , put
    , quit
    , resize
    )


port resize : (Int -> msg) -> Sub msg


port get : (String -> msg) -> Sub msg


port put : String -> Cmd msg


port quit : String -> Cmd msg
