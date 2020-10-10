module Table exposing
    ( Column
    , view
    )

import Format
import Justify exposing (Justify)
import List.Extra as List


type alias Column row =
    { title : Format.Node
    , justify : Justify
    , fromRow : Int -> row -> Format.Node
    }


type alias Config row =
    { includeHeader : Bool
    , columns : List (Column row)
    }


view : Config row -> List row -> List Format.Node
view config rows =
    let
        formatRow index row =
            List.map (\c -> c.fromRow index row) config.columns

        unjustified =
            let
                contentRows =
                    List.indexedMap formatRow rows
            in
            case config.includeHeader of
                False ->
                    contentRows

                True ->
                    let
                        headerRow =
                            List.map .title config.columns
                    in
                    headerRow :: contentRows

        columnSizes =
            let
                sizeRow index row =
                    List.getAt index row
                        |> Maybe.map Format.length
                        |> Maybe.withDefault 0

                longest index =
                    List.map (sizeRow index) unjustified
                        |> List.maximum
                        |> Maybe.withDefault 0

                indexes =
                    List.range 0 (List.length config.columns - 1)
            in
            List.map longest indexes

        justifyRow row =
            let
                justifyColumn index ( column, rowCell ) =
                    let
                        longest =
                            List.getAt index columnSizes
                                |> Maybe.withDefault 0
                    in
                    Justify.node column.justify longest rowCell
            in
            List.zip config.columns row
                |> List.indexedMap justifyColumn

        joinRow row =
            List.intersperse (Format.Span [] " ") row
                |> Format.Block
    in
    List.map justifyRow unjustified
        |> List.map joinRow
