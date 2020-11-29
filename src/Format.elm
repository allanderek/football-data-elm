module Format exposing
    ( Attribute
    , Node(..)
    , blankLine
    , format
    , length
    , nothing
    , text
    )

import Color


nothing : Node
nothing =
    Span [] ""


text : String -> Node
text =
    Span []


blankLine : Node
blankLine =
    nothing


type alias Attribute =
    Color.Attribute


type Node
    = Span (List Attribute) String
    | Block (List Node)


format : Node -> String
format node =
    case node of
        Block nodes ->
            List.map format nodes
                |> String.concat

        Span attributes content ->
            Color.colors attributes content


length : Node -> Int
length node =
    case node of
        Block nodes ->
            nodes
                |> List.map length
                |> List.sum

        Span _ s ->
            String.length s
