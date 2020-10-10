module Color exposing
    ( Command(..)
    , color
    , colors
    , command
    )


type Command
    = Reset
    | Bright
    | Dim
    | Underscore
    | Blink
    | Reverse
    | Hidden
    | FgBlack
    | FgRed
    | FgGreen
    | FgYellow
    | FgBlue
    | FgMagenta
    | FgCyan
    | FgWhite
    | BgBlack
    | BgRed
    | BgGreen
    | BgYellow
    | BgBlue
    | BgMagenta
    | BgCyan
    | BgWhite


colors : List Command -> String -> String
colors cs s =
    [ List.map command cs
        |> String.concat
    , s
    , command Reset
    ]
        |> String.concat


color : Command -> String -> String
color cs =
    colors [ cs ]


command : Command -> String
command c =
    case c of
        Reset ->
            "\u{001B}[0m"

        Bright ->
            "\u{001B}[1m"

        Dim ->
            "\u{001B}[2m"

        Underscore ->
            "\u{001B}[4m"

        Blink ->
            "\u{001B}[5m"

        Reverse ->
            "\u{001B}[7m"

        Hidden ->
            "\u{001B}[8m"

        FgBlack ->
            "\u{001B}[30m"

        FgRed ->
            "\u{001B}[31m"

        FgGreen ->
            "\u{001B}[32m"

        FgYellow ->
            "\u{001B}[33m"

        FgBlue ->
            "\u{001B}[34m"

        FgMagenta ->
            "\u{001B}[35m"

        FgCyan ->
            "\u{001B}[36m"

        FgWhite ->
            "\u{001B}[37m"

        BgBlack ->
            "\u{001B}[40m"

        BgRed ->
            "\u{001B}[41m"

        BgGreen ->
            "\u{001B}[42m"

        BgYellow ->
            "\u{001B}[43m"

        BgBlue ->
            "\u{001B}[44m"

        BgMagenta ->
            "\u{001B}[45m"

        BgCyan ->
            "\u{001B}[46m"

        BgWhite ->
            "\u{001B}[47m"
