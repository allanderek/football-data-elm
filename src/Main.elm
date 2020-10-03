module Main exposing (main)

import FootballData
import Helpers.Http as Http
import Helpers.Return as Return
import Http
import List.Extra as List
import Ports
import Private.Key
import Table


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


outputPage : Model -> ( Model, Cmd Msg )
outputPage model =
    drawPage model
        |> outputMessage model


init : Flags -> ( Model, Cmd Msg )
init _ =
    { page = PageWelcome
    , data =
        { standings = []
        , competitions = []
        , matches = []
        }
    , currentCompetition = 2021
    }
        |> outputPage


drawPage : Model -> String
drawPage model =
    case model.page of
        PageWelcome ->
            [ "Press 't' for table."
            , "Press 'c' for competitions."
            , "Press 'm' for matches"
            ]
                |> String.join "\n"

        PageCompetitions ->
            let
                formatSelected competition =
                    case competition.id == model.currentCompetition of
                        True ->
                            "X"

                        False ->
                            ""

                columns =
                    [ { title = ""
                      , justify = Table.CentreJustify
                      , fromRow = always formatSelected
                      }
                    , { title = ""
                      , justify = Table.RightJustify
                      , fromRow = \index _ -> index + 1 |> String.fromInt
                      }
                    , { title = "Region"
                      , justify = Table.LeftJustify
                      , fromRow = \_ r -> r.region
                      }
                    , { title = "Name"
                      , justify = Table.LeftJustify
                      , fromRow = \_ r -> r.name
                      }
                    ]
            in
            Table.view columns model.data.competitions

        PageTable ->
            FootballData.formatStandings model.data.standings

        PageMatches i ->
            let
                showScore match =
                    [ match.score.home |> Maybe.map String.fromInt |> Maybe.withDefault ""
                    , " - "
                    , match.score.away |> Maybe.map String.fromInt |> Maybe.withDefault ""
                    ]
                        |> String.concat

                columns =
                    [ { title = "Home"
                      , justify = Table.RightJustify
                      , fromRow = always .homeTeam
                      }
                    , { title = "Score"
                      , justify = Table.CentreJustify
                      , fromRow = always showScore
                      }
                    , { title = "Away"
                      , justify = Table.LeftJustify
                      , fromRow = always .awayTeam
                      }
                    ]

                screenRows =
                    30
            in
            model.data.matches
                |> List.drop i
                |> List.take screenRows
                |> Table.view columns


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.get Input


type alias Flags =
    ()


type alias Model =
    { data : ModelData
    , page : Page
    , currentCompetition : FootballData.CompetitionId
    }


withData : ModelData -> Model -> Model
withData data model =
    { model | data = data }


type Page
    = PageWelcome
    | PageCompetitions
    | PageMatches Int
    | PageTable


type alias ModelData =
    { standings : FootballData.Table
    , competitions : FootballData.Competitions
    , matches : FootballData.Matches
    }


type Msg
    = Input String
    | GetStandingsResponse (Http.Status FootballData.Table)
    | GetCompetitionsResponse (Http.Status FootballData.Competitions)
    | GetMatchesResponse (Http.Status FootballData.Matches)


updateCompetition : (Int -> Int) -> Model -> Model
updateCompetition moveI model =
    let
        isCurrent competition =
            competition.id == model.currentCompetition

        newCompetitionId =
            List.findIndex isCurrent model.data.competitions
                |> Maybe.map moveI
                |> Maybe.andThen (\i -> List.getAt i model.data.competitions)
                |> Maybe.map .id
                |> Maybe.withDefault model.currentCompetition
    in
    { model | currentCompetition = newCompetitionId }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetStandingsResponse (Err error) ->
            let
                message =
                    "I'm sorry there was some error getting the current standings"
            in
            [ message
            , Http.errorString error
            ]
                |> String.join "\n"
                |> outputMessage model

        GetStandingsResponse (Ok standings) ->
            let
                data =
                    model.data
            in
            model
                |> withData { data | standings = standings }
                |> outputPage

        GetCompetitionsResponse (Ok competitions) ->
            let
                data =
                    model.data
            in
            model
                |> withData { data | competitions = competitions }
                |> outputPage

        GetCompetitionsResponse (Err error) ->
            let
                message =
                    "I'm sorry there was some error getting the current competitions"
            in
            [ message
            , Http.errorString error
            ]
                |> String.join "\n"
                |> outputMessage model

        GetMatchesResponse (Ok matches) ->
            let
                data =
                    model.data
            in
            model
                |> withData { data | matches = matches }
                |> outputPage

        GetMatchesResponse (Err error) ->
            let
                message =
                    "I'm sorry there was some error getting the current matches"
            in
            [ message
            , Http.errorString error
            ]
                |> String.join "\n"
                |> outputMessage model

        Input "q" ->
            "Goodbye."
                |> Ports.quit
                |> Return.withModel model

        Input "c" ->
            FootballData.getCompetitions Private.Key.key GetCompetitionsResponse
                |> Return.withModel { model | page = PageCompetitions }

        Input "t" ->
            FootballData.getStandings Private.Key.key model.currentCompetition GetStandingsResponse
                |> Return.withModel { model | page = PageTable }

        Input "m" ->
            FootballData.getMatches Private.Key.key model.currentCompetition GetMatchesResponse
                |> Return.withModel { model | page = PageMatches 0 }

        Input "j" ->
            let
                newModel =
                    case model.page of
                        PageWelcome ->
                            model

                        PageTable ->
                            model

                        PageCompetitions ->
                            let
                                moveI i =
                                    (i + 1)
                                        |> min (List.length model.data.competitions)
                            in
                            updateCompetition moveI model

                        PageMatches i ->
                            { model
                                | page =
                                    (i + 1)
                                        |> min (List.length model.data.matches)
                                        |> PageMatches
                            }
            in
            outputPage newModel

        Input "k" ->
            let
                newModel =
                    case model.page of
                        PageWelcome ->
                            model

                        PageTable ->
                            model

                        PageCompetitions ->
                            let
                                moveI i =
                                    (i - 1)
                                        |> max 0
                            in
                            updateCompetition moveI model

                        PageMatches i ->
                            { model
                                | page =
                                    (i - 1)
                                        |> max 0
                                        |> PageMatches
                            }
            in
            outputPage newModel

        Input inputCommand ->
            [ "I'm sorry I do not understand that command: '"
            , inputCommand
            , "'"
            ]
                |> String.concat
                |> outputMessage model
