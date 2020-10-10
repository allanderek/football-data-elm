module FootballData exposing
    ( Competition
    , CompetitionId
    , Competitions
    , Match
    , Matches
    , Table
    , getCompetitions
    , getMatches
    , getStandings
    )

import Helpers.Decode as Decode
import Helpers.Http as Http
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import List.Extra as List
import Table
import Time


type alias Team =
    { name : TeamName
    , crestUrl : String
    }


type alias TeamName =
    String


type alias TablePosition =
    { team : Team
    , gamesPlayed : Int
    , form : String
    , won : Int
    , draw : Int
    , lost : Int
    , points : Int
    , goalsFor : Int
    , goalsAgainst : Int
    , goalDifference : Int
    }


type alias Table =
    List TablePosition


type alias Standings =
    { type_ : String
    , table : Table
    }


teamDecoder : Decoder Team
teamDecoder =
    Decode.succeed Team
        |> Decode.andField "name" Decode.string
        |> Decode.andField "crestUrl" Decode.string


standingsDecoder : Decoder Standings
standingsDecoder =
    let
        tablePositionDecoder =
            Decode.succeed TablePosition
                |> Decode.andField "team" teamDecoder
                |> Decode.andField "playedGames" Decode.int
                |> Decode.andMap (Decode.optionalNullableField "form" Decode.string |> Decode.map (Maybe.withDefault ""))
                |> Decode.andField "won" Decode.int
                |> Decode.andField "draw" Decode.int
                |> Decode.andField "lost" Decode.int
                |> Decode.andField "points" Decode.int
                |> Decode.andField "goalsFor" Decode.int
                |> Decode.andField "goalsAgainst" Decode.int
                |> Decode.andField "goalDifference" Decode.int
    in
    Decode.succeed Standings
        |> Decode.andField "type" Decode.string
        |> Decode.andField "table" (Decode.list tablePositionDecoder)


getStandings : String -> CompetitionId -> (Http.Status Table -> msg) -> Cmd msg
getStandings key competitionId toMessage =
    let
        allStandingsDecoder =
            Decode.field "standings" (Decode.list standingsDecoder)

        justTotal standings =
            let
                mTotals =
                    List.find (\s -> s.type_ == "TOTAL") standings
                        |> Maybe.map .table
            in
            case mTotals of
                Nothing ->
                    Decode.fail "The totals were not present."

                Just totals ->
                    Decode.succeed totals
    in
    { method = "GET"
    , headers = [ Http.header "X-Auth-Token" key ]
    , url =
        [ "https://api.football-data.org/v2/competitions/"
        , competitionId |> String.fromInt
        , "/standings"
        ]
            |> String.concat
    , body = Http.emptyBody
    , expect =
        allStandingsDecoder
            |> Decode.andThen justTotal
            |> Http.expectJson toMessage
    , timeout = Nothing
    , tracker = Nothing
    }
        |> Http.request




type alias Competitions =
    List Competition


type alias CompetitionId =
    Int


type alias Competition =
    { id : CompetitionId
    , name : String
    , region : String
    }


getCompetitions : String -> (Result Http.Error Competitions -> msg) -> Cmd msg
getCompetitions key toMessage =
    let
        competitionDecoder =
            Decode.succeed Competition
                |> Decode.andField "id" Decode.int
                |> Decode.andField "name" Decode.string
                |> Decode.andMap (Decode.at [ "area", "name" ] Decode.string)
    in
    { method = "GET"
    , headers = [ Http.header "X-Auth-Token" key ]
    , url = "https://api.football-data.org/v2/competitions?plan=TIER_ONE"
    , body = Http.emptyBody
    , expect =
        competitionDecoder
            |> Decode.list
            |> Decode.field "competitions"
            |> Http.expectJson toMessage
    , timeout = Nothing
    , tracker = Nothing
    }
        |> Http.request


type alias Matches =
    List Match


type alias Match =
    { homeTeam : TeamName
    , awayTeam : TeamName
    , score : Score
    , utcDateTime : Time.Posix
    }


type alias Score =
    { home : Maybe Int
    , away : Maybe Int
    }


getMatches : String -> CompetitionId -> (Http.Status Matches -> msg) -> Cmd msg
getMatches key competitionId toMessage =
    let
        scoreDecoder =
            Decode.succeed Score
                |> Decode.andField "homeTeam" (Decode.maybe Decode.int)
                |> Decode.andField "awayTeam" (Decode.maybe Decode.int)

        matchDecoder =
            Decode.succeed Match
                |> Decode.andFieldAt [ "homeTeam", "name" ] Decode.string
                |> Decode.andFieldAt [ "awayTeam", "name" ] Decode.string
                |> Decode.andFieldAt [ "score", "fullTime" ] scoreDecoder
                |> Decode.andField "utcDate" Iso8601.decoder
    in
    { method = "GET"
    , headers = [ Http.header "X-Auth-Token" key ]
    , url =
        [ "https://api.football-data.org/v2/competitions/"
        , competitionId |> String.fromInt
        , "/matches"
        ]
            |> String.concat
    , body = Http.emptyBody
    , expect =
        matchDecoder
            |> Decode.list
            |> Decode.field "matches"
            |> Http.expectJson toMessage
    , timeout = Nothing
    , tracker = Nothing
    }
        |> Http.request
