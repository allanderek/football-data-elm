module FootballData exposing
    ( Table
    , formatStandings
    , getStandings
    )

import Helpers.Decode as Decode
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode
import List.Extra as List


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


getStandings : String -> (Result Http.Error Table -> msg) -> Cmd msg
getStandings key toMessage =
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
    , url = "https://api.football-data.org/v2/competitions/2021/standings"
    , body = Http.emptyBody
    , expect =
        allStandingsDecoder
            |> Decode.andThen justTotal
            |> Http.expectJson toMessage
    , timeout = Nothing
    , tracker = Nothing
    }
        |> Http.request


formatStandings : Table -> String
formatStandings table =
    "I'm supposed to format a table."
