module Main exposing (main)

import Color
import Dict
import FootballData
import Format
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
import TimeZone


type alias Model =
    { data : ModelData
    , standingsRequestStatus : RequestStatus
    , competitionsRequestStatus : RequestStatus
    , matchesRequestStatus : RequestStatus
    , page : Page
    , currentCompetition : FootballData.CompetitionId
    , screenRows : Int
    , screenColumns : Int
    , here : Time.Zone
    , tickTock : TickTock
    }


type TickTock
    = Tick
    | Tock


withData : ModelData -> Model -> Model
withData data model =
    { model | data = data }


type alias Scroll =
    Int


type Page
    = PageCompetitions
    | PageMatches Scroll
    | PageTable Scroll


type alias ModelData =
    { standings : List FootballData.Table
    , competitions : FootballData.Competitions
    , matches : FootballData.Matches
    }


type RequestStatus
    = Inflight
    | Ready


presetKnownCompetitionIds :
    { epl : FootballData.CompetitionId
    , laliga : FootballData.CompetitionId
    , bundesliga : FootballData.CompetitionId
    , seriea : FootballData.CompetitionId
    , ucl : FootballData.CompetitionId
    }
presetKnownCompetitionIds =
    { epl = 2021
    , laliga = 2014
    , bundesliga = 2002
    , seriea = 2019
    , ucl = 2001
    }


main : Program ProgramFlags Model Msg
main =
    let
        subscriptions model =
            [ Ports.get Input
            , Ports.resize Resize
            , case relevantRequestStatus model of
                Ready ->
                    Sub.none

                Inflight ->
                    Time.every 500 (always TickTock)
            ]
                |> Sub.batch

        init flags =
            { here =
                -- TODO: Of course we should allow passing this in as an argument.
                case Dict.get "Europe/London" TimeZone.zones of
                    Just getZone ->
                        getZone ()

                    Nothing ->
                        Time.utc
            , page = PageMatches 0
            , data =
                { standings = []
                , competitions = []
                , matches = []
                }
            , standingsRequestStatus = Ready
            , competitionsRequestStatus = Ready
            , matchesRequestStatus = Ready
            , currentCompetition = presetKnownCompetitionIds.epl
            , screenRows =
                ProgramFlags.decodeFlag flags 30 "rows" Decode.int
            , screenColumns =
                ProgramFlags.decodeFlag flags 30 "columns" Decode.int
            , tickTock = Tick
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


relevantRequestStatus : Model -> RequestStatus
relevantRequestStatus model =
    case model.page of
        PageCompetitions ->
            model.competitionsRequestStatus

        PageMatches _ ->
            model.matchesRequestStatus

        PageTable _ ->
            model.standingsRequestStatus


drawPage : Model -> String
drawPage model =
    let
        keySpan =
            Format.Span [ Color.Underscore ]

        normal =
            Format.Span []

        workingIndicator =
            let
                character =
                    case model.tickTock of
                        Tick ->
                            "/"

                        Tock ->
                            "\\"
            in
            case relevantRequestStatus model of
                Ready ->
                    Format.text " "

                Inflight ->
                    Format.Span
                        [ Color.FgRed
                        , Color.Bright
                        ]
                        character

        menu =
            [ [ keySpan "t", normal "able" ]
            , [ keySpan "m", normal "atches" ]
            , [ keySpan "c", normal "ompetitions" ]
            , [ keySpan "j", normal "up" ]
            , [ keySpan "k", normal "down" ]
            , [ normal "e", keySpan "p", normal "l" ]
            , [ keySpan "l", normal "aliga" ]
            , [ keySpan "b", normal "undesliga" ]
            , [ keySpan "s", normal "eriea" ]
            , [ keySpan "u", normal "cl" ]
            ]
                |> List.map Format.Block
                |> List.intersperse (normal ", ")
                |> Format.Block
                -- -1 because we need to leave space for the working indicator
                |> Justify.node Justify.Centre (model.screenColumns - 1)

        header =
            [ workingIndicator
            , menu
            ]
                |> Format.Block
                |> Format.format

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


centreJustifyScreen : { a | screenColumns : Int, screenRows : Int } -> List Format.Node -> List Format.Node
centreJustifyScreen model nodes =
    let
        justify =
            Justify.node Justify.Centre model.screenColumns
    in
    List.map justify nodes
        |> Justify.vertical Justify.Centre model.screenRows Format.nothing


showScrolled : { a | screenRows : Int } -> Int -> List Format.Node -> List Format.Node
showScrolled model scroll nodes =
    let
        headerSize =
            1

        screenRows =
            model.screenRows - headerSize
    in
    nodes
        |> List.drop scroll
        |> List.take screenRows


drawPageContents : Model -> String
drawPageContents model =
    case model.page of
        PageCompetitions ->
            let
                formatSelected competition =
                    case competition.id == model.currentCompetition of
                        True ->
                            Format.Span
                                [ Color.Bright
                                , Color.FgGreen
                                ]
                                "X"

                        False ->
                            Format.nothing

                columns =
                    [ { title = Format.text ""
                      , justify = Justify.Centre
                      , fromRow = always formatSelected
                      }
                    , { title = Format.text ""
                      , justify = Justify.Right
                      , fromRow = \index _ -> index + 1 |> String.fromInt |> Format.text
                      }
                    , { title = Format.text "Region"
                      , justify = Justify.Left
                      , fromRow = \_ r -> r.region |> Format.text
                      }
                    , { title = Format.text "Name"
                      , justify = Justify.Left
                      , fromRow = \_ r -> r.name |> Format.text
                      }
                    ]
            in
            model.data.competitions
                |> List.map Table.NormalRow
                |> Table.view
                    { columns = columns
                    , includeHeader = True
                    }
                |> centreJustifyScreen model
                |> List.map Format.format
                |> String.join "\n"

        PageTable scroll ->
            let
                allStandingLines =
                    model.data.standings
                        |> List.map formatStandings
                        |> List.intersperse [ Format.blankLine ]
                        |> List.concat
            in
            allStandingLines
                |> showScrolled model scroll
                |> centreJustifyScreen model
                |> List.map Format.format
                |> String.join "\n"

        PageMatches scroll ->
            let
                groups =
                    FootballData.groupMatchesByDate model.here model.data.matches

                showGroup ( principal, others ) =
                    let
                        header =
                            Time.formatDate model.here principal.utcDateTime
                                |> Format.Span [ Color.Dim ]
                                |> Table.BannerRow

                        matchesTable =
                            (principal :: others)
                                |> List.map Table.NormalRow
                    in
                    header :: matchesTable

                showScore match =
                    let
                        showNumber mI =
                            -- We pad this
                            mI |> Maybe.map String.fromInt |> Maybe.withDefault ""

                        score =
                            [ match.score.home |> showNumber |> String.padLeft 2 ' '
                            , " - "
                            , match.score.away |> showNumber |> String.padRight 2 ' '
                            ]
                                |> String.concat
                    in
                    case match.status of
                        -- Note: The scores are 7 characters. Generally they are 5 character eg "0 - 0"
                        -- so we pad them on either side just to give a bit more space *and* to allow for a
                        -- double digit score, eg 10 - 0.
                        -- then of course it's more.
                        -- So that means that the 'word' statuses are better being odd-numbered characters since
                        -- it means they will centre justify better.
                        FootballData.Postponed ->
                            "PSTPD"
                                |> Format.Span [ Color.FgRed, Color.Bright ]

                        FootballData.Scheduled ->
                            Time.formatTime model.here match.utcDateTime
                                -- We get a mild bit of jankiness when you are viewing *only* matches with 'time'
                                -- scores and scroll up so that you suddenly view a match with a score, because
                                -- the time status would be 5 characters but the score ones are 7 characters.
                                -- This would not be needed if we always rendered *all* the matches.
                                |> String.pad 7 ' '
                                |> Format.text

                        FootballData.Cancelled ->
                            "CNCLD"
                                |> Format.Span [ Color.FgRed, Color.Bright ]

                        FootballData.Suspended ->
                            "SUSPD"
                                |> Format.Span [ Color.FgRed, Color.Bright ]

                        FootballData.Playing ->
                            score
                                |> Format.Span [ Color.FgBlue, Color.Bright ]

                        FootballData.Paused ->
                            score
                                |> Format.Span [ Color.FgYellow, Color.Bright ]

                        FootballData.Finished ->
                            score
                                |> Format.text

                        FootballData.Awarded ->
                            score
                                |> Format.text

                formatLoser s =
                    s |> Format.text

                formatDrawer s =
                    s |> Format.text

                formatWinner s =
                    s |> Format.Span [ Color.Bright ]

                formatTeamName name score otherScore =
                    case compare (Maybe.withDefault 0 score) (Maybe.withDefault 0 otherScore) of
                        LT ->
                            formatLoser name

                        EQ ->
                            formatDrawer name

                        GT ->
                            formatWinner name

                formatHomeTeam match =
                    formatTeamName match.homeTeam match.score.home match.score.away

                formatAwayTeam match =
                    formatTeamName match.awayTeam match.score.away match.score.home

                columns =
                    [ { title = Format.text "Home"
                      , justify = Justify.Right
                      , fromRow = always formatHomeTeam
                      }
                    , { title = Format.text "Score"
                      , justify = Justify.Centre
                      , fromRow = always showScore
                      }
                    , { title = Format.text "Away"
                      , justify = Justify.Left
                      , fromRow = always formatAwayTeam
                      }
                    ]
            in
            List.map showGroup groups
                |> List.concat
                |> Table.view
                    { includeHeader = False
                    , columns = columns
                    }
                |> showScrolled model scroll
                |> centreJustifyScreen model
                |> List.map Format.format
                |> String.join "\n"


type Msg
    = Input String
    | Resize Decode.Value
    | TickTock
    | GetStandingsResponse (Http.Status (List FootballData.Table))
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


presetCompetition : FootballData.CompetitionId -> Model -> ( Model, Cmd Msg )
presetCompetition newCompetitionId model =
    let
        newModel =
            { model | currentCompetition = newCompetitionId }
    in
    case model.page of
        PageCompetitions ->
            Return.noCommand newModel

        PageMatches _ ->
            newModel |> gotoMatches

        PageTable _ ->
            newModel |> gotoTable


gotoMatches : Model -> ( Model, Cmd Msg )
gotoMatches model =
    let
        matchesScroll =
            case model.page of
                PageMatches i ->
                    i

                PageCompetitions ->
                    0

                PageTable _ ->
                    0
    in
    FootballData.getMatches Private.Key.key model.currentCompetition GetMatchesResponse
        |> Return.withModel { model | page = PageMatches matchesScroll, matchesRequestStatus = Inflight }
        |> Return.combine outputPage


gotoTable : Model -> ( Model, Cmd Msg )
gotoTable model =
    let
        tableScroll =
            case model.page of
                PageTable i ->
                    i

                PageCompetitions ->
                    0

                PageMatches _ ->
                    0
    in
    FootballData.getStandings Private.Key.key model.currentCompetition GetStandingsResponse
        |> Return.withModel { model | page = PageTable tableScroll, standingsRequestStatus = Inflight }
        |> Return.combine outputPage


gotoCompetitions : Model -> ( Model, Cmd Msg )
gotoCompetitions model =
    FootballData.getCompetitions Private.Key.key GetCompetitionsResponse
        |> Return.withModel { model | page = PageCompetitions, competitionsRequestStatus = Inflight }
        |> Return.combine outputPage


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

        TickTock ->
            { model
                | tickTock =
                    case model.tickTock of
                        Tick ->
                            Tock

                        Tock ->
                            Tick
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
                |> outputMessage { model | standingsRequestStatus = Ready }

        GetStandingsResponse (Ok standings) ->
            let
                data =
                    model.data
            in
            { model | standingsRequestStatus = Ready }
                |> withData { data | standings = standings }
                |> outputPage

        GetCompetitionsResponse (Ok competitions) ->
            let
                data =
                    model.data
            in
            { model | competitionsRequestStatus = Ready }
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
                |> outputMessage { model | competitionsRequestStatus = Ready }

        GetMatchesResponse (Ok matches) ->
            let
                data =
                    model.data

                newPage =
                    case model.page of
                        PageMatches 0 ->
                            let
                                scroll x groups =
                                    case groups of
                                        [] ->
                                            -- So in this case we got here without seeing a scheduled match
                                            -- so we basically just want to scroll to the bottom.
                                            max 0 (x - model.screenRows)

                                        ( principal, others ) :: rest ->
                                            let
                                                isDesired m =
                                                    case m.status of
                                                        FootballData.Scheduled ->
                                                            True

                                                        FootballData.Playing ->
                                                            True

                                                        FootballData.Paused ->
                                                            True

                                                        FootballData.Postponed ->
                                                            False

                                                        FootballData.Cancelled ->
                                                            False

                                                        FootballData.Suspended ->
                                                            False

                                                        FootballData.Finished ->
                                                            False

                                                        FootballData.Awarded ->
                                                            False

                                                thisGroupMatches =
                                                    principal :: others
                                            in
                                            case List.findIndex isDesired thisGroupMatches of
                                                Nothing ->
                                                    let
                                                        groupSize =
                                                            List.length thisGroupMatches
                                                    in
                                                    -- The plus one is for the data header
                                                    scroll (x + 1 + groupSize) rest

                                                Just i ->
                                                    -- Note: no + 1 here, because we *want* this group's header
                                                    -- to be included.
                                                    x + i
                            in
                            FootballData.groupMatchesByDate model.here matches
                                |> scroll 0
                                |> PageMatches

                        current ->
                            current
            in
            { model | page = newPage, matchesRequestStatus = Ready }
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
                |> outputMessage { model | matchesRequestStatus = Ready }

        Input "q" ->
            "Goodbye."
                |> Ports.quit
                |> Return.withModel model

        Input "c" ->
            gotoCompetitions model

        Input "t" ->
            gotoTable model

        Input "m" ->
            gotoMatches model

        Input "p" ->
            presetCompetition presetKnownCompetitionIds.epl model

        Input "l" ->
            presetCompetition presetKnownCompetitionIds.laliga model

        Input "b" ->
            presetCompetition presetKnownCompetitionIds.bundesliga model

        Input "s" ->
            presetCompetition presetKnownCompetitionIds.seriea model

        Input "u" ->
            presetCompetition presetKnownCompetitionIds.ucl model

        Input "j" ->
            let
                newModel =
                    case model.page of
                        PageTable scroll ->
                            { model
                                | page =
                                    (scroll + 1)
                                        -- TODO: it would be great to minmise to not allow scrolling off the page
                                        |> PageTable
                            }

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
                        PageTable scroll ->
                            { model
                                | page =
                                    (scroll - 1)
                                        |> max 0
                                        |> PageTable
                            }

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


formatStandings : FootballData.Table -> List Format.Node
formatStandings table =
    let
        integerFormat get _ row =
            -- TODO: Could color these red for negative etc.
            get row |> String.fromInt |> Format.text

        showRes c =
            case c of
                'W' ->
                    Format.Span [ Color.FgGreen ] "W"

                'D' ->
                    Format.Span [ Color.FgYellow ] "D"

                'L' ->
                    Format.Span [ Color.FgRed ] "L"

                _ ->
                    String.fromChar c |> Format.text

        showForm _ pos =
            pos.form
                |> String.toList
                |> List.reverse
                |> List.map showRes
                |> Format.Block

        columns =
            [ { title = Format.text "P"
              , justify = Justify.Right
              , fromRow = \index _ -> index + 1 |> String.fromInt |> Format.text
              }
            , { title = Format.text "Team"
              , justify = Justify.Left
              , fromRow = \_ r -> r.team.name |> Format.text
              }
            , { title = Format.text "Pld"
              , justify = Justify.Right
              , fromRow = integerFormat .gamesPlayed
              }
            , { title = Format.text "W"
              , justify = Justify.Right
              , fromRow = integerFormat .won
              }
            , { title = Format.text "D"
              , justify = Justify.Right
              , fromRow = integerFormat .draw
              }
            , { title = Format.text "L"
              , justify = Justify.Right
              , fromRow = integerFormat .lost
              }
            , { title = Format.text "GF"
              , justify = Justify.Right
              , fromRow = integerFormat .goalsFor
              }
            , { title = Format.text "GA"
              , justify = Justify.Right
              , fromRow = integerFormat .goalsAgainst
              }
            , { title = Format.text "GD"
              , justify = Justify.Right
              , fromRow = integerFormat .goalDifference
              }
            , { title = Format.text "Pts"
              , justify = Justify.Right
              , fromRow = integerFormat .points
              }
            , { title = Format.text "Form"
              , justify = Justify.Right
              , fromRow = showForm
              }
            ]
    in
    table
        |> List.map Table.NormalRow
        |> Table.view
            { columns = columns
            , includeHeader = True
            }
