module Helpers.Time exposing
    ( toEnglishMonth
    , toIntMonth
    , formatDate
    )

import Time exposing (Zone, Posix, Month(..))


toEnglishMonth : Month -> String
toEnglishMonth month =
    case month of
        Jan -> "January"
        Feb -> "February"
        Mar -> "March"
        Apr -> "April"
        May -> "May"
        Jun -> "June"
        Jul -> "July"
        Aug -> "August"
        Sep -> "september"
        Oct -> "October"
        Nov -> "November"
        Dec -> "December"

toIntMonth : Month -> Int
toIntMonth month =
    case month of
        Jan -> 1
        Feb -> 2
        Mar -> 3
        Apr -> 4
        May -> 5
        Jun -> 6
        Jul -> 7
        Aug -> 8
        Sep -> 9
        Oct -> 10
        Nov -> 11
        Dec -> 12

formatDate : Zone -> Posix -> String
formatDate zone posix =
    [ posix |> Time.toYear zone |> String.fromInt |> String.padLeft 4 '0' 
    , posix |> Time.toMonth zone |> toIntMonth |> String.fromInt |> String.padLeft 2 '0'
    , posix |> Time.toDay zone |> String.fromInt |> String.padLeft 2 '0'
    ]
        |> String.join "/"

