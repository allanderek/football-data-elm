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


outputMessage : Model -> String -> ( Model, Cmd Msg )
outputMessage model output =
    output
        |> Ports.put
        |> Return.withModel model


init : Flags -> ( Model, Cmd Msg )
init model =
    outputMessage model "Press 't' for table."


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
    case msg of
        GetStandingsResponse (Err error) ->
            let
                errorString =
                    case error of
                        Http.BadUrl s ->
                            String.append "Bad url: " s

                        Http.Timeout ->
                            "Timeout"

                        Http.NetworkError ->
                            "Network error"

                        Http.BadStatus i ->
                            String.fromInt i
                                |> String.append "Bad status: "

                        Http.BadBody s ->
                            String.append "Bad body: " s

                message =
                    "I'm sorry there was some error getting the current standings"
            in
            [ message
            , errorString
            ]
                |> String.join "\n"
                |> outputMessage model

        GetStandingsResponse (Ok standings) ->
            FootballData.formatStandings standings
                |> outputMessage model

        Input "q" ->
            "Goodbye."
                |> Ports.quit
                |> Return.withModel model

        Input "t" ->
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
