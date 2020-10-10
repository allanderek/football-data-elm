module Main exposing (main)

import Helpers.Time as Time
import FootballData
import Helpers.Http as Http
import Helpers.Return as Return
import Http
import Json.Decode as Decode
import List.Extra as List
import Ports
import Private.Key
import ProgramFlags exposing (ProgramFlags)
import Table
import Time


main : Program ProgramFlags Model Msg
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


init : ProgramFlags -> ( Model, Cmd Msg )
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
    }
        |> gotoMatches
        |> Return.combine outputPage


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
            model.data.competitions
                |> Table.view
                    { columns = columns
                    , includeHeader = True
                    }
                |> String.join "\n"

        PageTable ->
            FootballData.formatStandings model.data.standings

        PageMatches i ->
            let
                groups =
                    -- This isn't quite right, you don't want the 'dateTime' to be equal
                    -- but to be on the same date.
                    List.gatherEqualsBy .utcDateTime model.data.matches

                showGroup (principal, others) =
                    let
                        header =
                            Time.formatDate model.here principal.utcDateTime

                        matchesTable =
                            (principal :: others)
                                    |> Table.view
                                        { columns = columns
                                        , includeHeader = False
                                        }
                    in
                    header ::  matchesTable

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
                    -- minus 1 is intended to be for the header
                    model.screenRows - 1

                rowsToShow =
                    List.map showGroup groups
                        |> List.concat
                        |> List.drop i
                        |> List.take screenRows

                justify =
                    String.pad 70 ' '
            in
            rowsToShow
                |> List.map justify
                |> String.join "\n"


subscriptions : Model -> Sub Msg
subscriptions _ =
    [ Ports.get Input
    , Ports.resize Resize
    ]
        |> Sub.batch


type alias Model =
    { data : ModelData
    , page : Page
    , currentCompetition : FootballData.CompetitionId
    , screenRows : Int
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


type Msg
    = Input String
    | Resize Int
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
        Resize rows ->
            { model | screenRows = rows }
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
