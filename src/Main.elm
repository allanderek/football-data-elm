module Main exposing (main)

import FootballData
import Helpers.Return as Return
import Http
import Ports
import Private.Key


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    Return.noCommand ()


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.get Input


type alias Flags =
    ()


type alias Model =
    ()


type Msg
    = Input String
    | GetStandingsResponse (Result Http.Error FootballData.Table)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        outputMessage m s =
            s
                |> Ports.put
                |> Return.withModel m
    in
    case msg of
        GetStandingsResponse (Err _) ->
            -- "I'm sorry there was some error getting the current standings"
            -- "Hello goodbye"
            --     |> outputMessage model
            Return.noCommand model

        GetStandingsResponse (Ok standings) ->
            -- FootballData.formatStandings standings
            -- "I'm supposed to be table output"
            --    |> outputMessage model
            Return.noCommand model

        Input "table\n" ->
            [ FootballData.getStandings Private.Key.key GetStandingsResponse
            ]
                |> Cmd.batch
                |> Return.withModel model

        Input inputCommand ->
            [ "I'm sorry I do not understand that command: '"
            , inputCommand
            , "'"
            ]
                |> String.concat
                |> outputMessage model
