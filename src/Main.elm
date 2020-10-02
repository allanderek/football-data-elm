module Main exposing (main)

import Ports
import Helpers.Return as Return

main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
init : Flags -> ( Model , Cmd Msg)
init _ =
    Return.noCommand ()
subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.get Input

type alias Flags = ()
type alias Model = ()

type Msg
    = Input String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            let
                command =
                    input
                        |> String.reverse
                        |> Ports.put
            in
            Return.withCommand command model

