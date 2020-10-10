module Justify exposing
    ( Justify(..)
    , string
    )

type Justify
    = Left
    | Right
    | Centre
    | None

string : Justify -> Int -> String -> String
string justify width input =
    case justify of
        None ->
            input

        Right ->
            String.padLeft width ' ' input

        Left ->
            String.padRight width ' ' input

        Centre ->
            String.pad width ' ' input

