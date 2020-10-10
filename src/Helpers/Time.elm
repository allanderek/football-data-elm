module Helpers.Time exposing
    ( formatDate
    , getDate
    , toEnglishMonth
    , toIntMonth
    )

import Time exposing (Month(..), Posix, Zone)


toEnglishMonth : Month -> String
toEnglishMonth month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "september"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


toIntMonth : Month -> Int
toIntMonth month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


getDate : Zone -> Posix -> { year : Int, month : Month, day : Int }
getDate zone posix =
    { year = Time.toYear zone posix
    , month = Time.toMonth zone posix
    , day = Time.toDay zone posix
    }


formatDate : Zone -> Posix -> String
formatDate zone posix =
    let
        date =
            getDate zone posix
    in
    [ date.year |> String.fromInt |> String.padLeft 4 '0'
    , date.month |> toIntMonth |> String.fromInt |> String.padLeft 2 '0'
    , date.day |> String.fromInt |> String.padLeft 2 '0'
    ]
        |> String.join "/"
