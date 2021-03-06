module Justify exposing
    ( Justify(..)
    , node
    , string
    , vertical
    )

import Format


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


node : Justify -> Int -> Format.Node -> Format.Node
node justify width inputNode =
    let
        currentLength =
            Format.length inputNode

        pad length =
            String.repeat length " "
                |> Format.Span []
    in
    case justify of
        None ->
            inputNode

        Right ->
            [ pad (width - currentLength)
            , inputNode
            ]
                |> Format.Block

        Left ->
            [ inputNode
            , pad (width - currentLength)
            ]
                |> Format.Block

        Centre ->
            let
                ( left, right ) =
                    splitPadding currentLength width
            in
            [ pad left
            , inputNode
            , pad right
            ]
                |> Format.Block


vertical : Justify -> Int -> a -> List a -> List a
vertical justify height empty lines =
    let
        currentHeight =
            List.length lines

        pad length =
            List.repeat length empty
    in
    case justify of
        None ->
            lines

        Right ->
            [ pad (height - currentHeight)
            , lines
            ]
                |> List.concat

        Left ->
            [ lines
            , pad (height - currentHeight)
            ]
                |> List.concat

        Centre ->
            let
                ( upper, lower ) =
                    splitPadding currentHeight height
            in
            [ pad upper
            , lines
            , pad lower
            ]
                |> List.concat


splitPadding : Int -> Int -> ( Int, Int )
splitPadding currentSize desiredSize =
    let
        padRequired =
            desiredSize - currentSize

        prior =
            padRequired // 2

        post =
            -- So if it is an odd number the extra part will go on the right.
            padRequired - prior
    in
    ( prior, post )
