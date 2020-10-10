module Table exposing
    ( Column
    , view
    )

import List.Extra as List
import Justify exposing (Justify)


type alias Column row =
    { title : String
    , justify : Justify
    , fromRow : Int -> row -> String
    }



type alias Config row =
    { includeHeader : Bool
    , columns : List (Column row)
    }


view : Config row -> List row -> List String
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
                        |> Maybe.map String.length
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
                    Justify.string column.justify longest rowCell
            in
            List.zip config.columns row
                |> List.indexedMap justifyColumn
    in
    List.map justifyRow unjustified
        |> List.map (String.join " ")
