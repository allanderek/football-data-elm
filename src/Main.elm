module Main exposing (main)

import Color
import FootballData
import Helpers.Http as Http
import Helpers.Return as Return
import Helpers.Time as Time
import Http
import Json.Decode as Decode
import Justify
import List.Extra as List
import Ports
import Private.Key
import ProgramFlags exposing (ProgramFlags)
import Table
import Time


type alias Model =
    { data : ModelData
    , page : Page
    , currentCompetition : FootballData.CompetitionId
    , screenRows : Int
    , screenColumns : Int
    , here : Time.Zone
    }


withData : ModelData -> Model -> Model
withData data model =
    { model | data = data }


type Page
    = PageCompetitions
    | PageMatches Int
    | PageTable


type alias ModelData =
    { standings : FootballData.Table
    , competitions : FootballData.Competitions
    , matches : FootballData.Matches
    }


main : Program ProgramFlags Model Msg
main =
    let
        subscriptions _ =
            [ Ports.get Input
            , Ports.resize Resize
            ]
                |> Sub.batch

        init flags =
            { here = Time.utc
            , page = PageMatches 0
            , data =
                { standings = []
                , competitions = []
                , matches = []
                }
            , currentCompetition = 2021
            , screenRows =
                ProgramFlags.decodeFlag flags 30 "rows" Decode.int
            , screenColumns =
                ProgramFlags.decodeFlag flags 30 "columns" Decode.int
            }
                |> gotoMatches
                |> Return.combine outputPage
    in
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


drawPage : Model -> String
drawPage model =
    let
        header =
            "(t)able, (m)atches, (c)ompetitions, (j)up, (k)down"

        contentSpace =
            model.screenRows
                - 1
                |> max 0

        pageContents =
            drawPageContents { model | screenRows = contentSpace }
    in
    [ header
    , pageContents
    ]
        |> String.join "\n"


drawPageContents : Model -> String
drawPageContents model =
    case model.page of
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
                      , justify = Justify.Centre
                      , fromRow = always formatSelected
                      }
                    , { title = ""
                      , justify = Justify.Right
                      , fromRow = \index _ -> index + 1 |> String.fromInt
                      }
                    , { title = "Region"
                      , justify = Justify.Left
                      , fromRow = \_ r -> r.region
                      }
                    , { title = "Name"
                      , justify = Justify.Left
                      , fromRow = \_ r -> r.name
                      }
                    ]
            in
            model.data.competitions
                |> Table.view
                    { columns = columns
                    , includeHeader = True
                    }
                |> String.join "\n"

        PageTable ->
            formatStandings model.data.standings

        PageMatches i ->
            let
                groups =
                    -- This isn't quite right, you don't want the 'dateTime' to be equal
                    -- but to be on the same date.
                    List.gatherEqualsBy (.utcDateTime >> Time.getDate model.here) model.data.matches

                showGroup ( principal, others ) =
                    let
                        header =
                            Time.formatDate model.here principal.utcDateTime
                                |> Color.color Color.Dim

                        matchesTable =
                            (principal :: others)
                                |> Table.view
                                    { columns = columns
                                    , includeHeader = False
                                    }
                    in
                    header :: matchesTable

                showScore match =
                    let
                        score =
                            [ match.score.home |> Maybe.map String.fromInt |> Maybe.withDefault ""
                            , " - "
                            , match.score.away |> Maybe.map String.fromInt |> Maybe.withDefault ""
                            ]
                                |> String.concat
                    in
                    case match.status of
                        FootballData.Postponed ->
                            "POSP"
                                |> Color.colors [ Color.FgRed, Color.Bright ]

                        FootballData.Scheduled ->
                            Time.formatTime model.here match.utcDateTime

                        FootballData.Cancelled ->
                            "CANC"
                                |> Color.colors [ Color.FgRed, Color.Bright ]

                        FootballData.Suspended ->
                            "SUSP"
                                |> Color.colors [ Color.FgRed, Color.Bright ]

                        FootballData.Playing ->
                            score
                                |> Color.colors [ Color.FgBlue, Color.Bright ]

                        FootballData.Paused ->
                            score
                                |> Color.color Color.FgYellow

                        FootballData.Finished ->
                            score

                        FootballData.Awarded ->
                            score

                columns =
                    [ { title = "Home"
                      , justify = Justify.Right
                      , fromRow = always .homeTeam
                      }
                    , { title = "Score"
                      , justify = Justify.Centre
                      , fromRow = always showScore
                      }
                    , { title = "Away"
                      , justify = Justify.Left
                      , fromRow = always .awayTeam
                      }
                    ]

                screenRows =
                    -- minus 1 is intended to be for the header
                    model.screenRows - 1

                rowsToShow =
                    List.map showGroup groups
                        |> List.concat
                        |> List.drop i
                        |> List.take screenRows

                justify =
                    String.pad model.screenColumns ' '
            in
            rowsToShow
                |> List.map justify
                |> String.join "\n"


type Msg
    = Input String
    | Resize Decode.Value
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


gotoMatches : Model -> ( Model, Cmd Msg )
gotoMatches model =
    FootballData.getMatches Private.Key.key model.currentCompetition GetMatchesResponse
        |> Return.withModel { model | page = PageMatches 0 }


gotoTable : Model -> ( Model, Cmd Msg )
gotoTable model =
    FootballData.getStandings Private.Key.key model.currentCompetition GetStandingsResponse
        |> Return.withModel { model | page = PageTable }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize value ->
            let
                get default fieldName =
                    Decode.decodeValue (Decode.field fieldName Decode.int) value
                        |> Result.withDefault default
            in
            { model
                | screenRows = get model.screenRows "rows"
                , screenColumns = get model.screenColumns "columns"
            }
                |> outputPage

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
            gotoTable model

        Input "m" ->
            gotoMatches model

        Input "j" ->
            let
                newModel =
                    case model.page of
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


formatStandings : FootballData.Table -> String
formatStandings table =
    let
        integerFormat get _ row =
            get row |> String.fromInt

        columns =
            [ { title = "P"
              , justify = Justify.Right
              , fromRow = \index _ -> index + 1 |> String.fromInt
              }
            , { title = "Team"
              , justify = Justify.Left
              , fromRow = \_ r -> r.team.name
              }
            , { title = "Pld"
              , justify = Justify.Right
              , fromRow = integerFormat .gamesPlayed
              }
            , { title = "W"
              , justify = Justify.Right
              , fromRow = integerFormat .won
              }
            , { title = "D"
              , justify = Justify.Right
              , fromRow = integerFormat .draw
              }
            , { title = "L"
              , justify = Justify.Right
              , fromRow = integerFormat .lost
              }
            , { title = "GF"
              , justify = Justify.Right
              , fromRow = integerFormat .goalsFor
              }
            , { title = "GA"
              , justify = Justify.Right
              , fromRow = integerFormat .goalsAgainst
              }
            , { title = "GD"
              , justify = Justify.Right
              , fromRow = integerFormat .goalDifference
              }
            , { title = "Pts"
              , justify = Justify.Right
              , fromRow = integerFormat .points
              }
            ]
    in
    table
        |> Table.view
            { columns = columns
            , includeHeader = True
            }
        |> String.join "\n"
