module Table exposing
    ( Column
    , Row(..)
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


type Row row
    = NormalRow row
    | BannerRow Format.Node


view : Config row -> List (Row row) -> List Format.Node
view config rows =
    let
        formatRow : Int -> Row row -> Row (List Format.Node)
        formatRow index row =
            case row of
                NormalRow rowContents ->
                    List.map (\c -> c.fromRow index rowContents) config.columns
                        |> NormalRow

                BannerRow content ->
                    BannerRow content

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
                                |> NormalRow
                    in
                    headerRow :: contentRows

        columnSizes =
            let
                sizeRow index row =
                    case row of
                        BannerRow _ ->
                            0

                        NormalRow rowContents ->
                            List.getAt index rowContents
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
            case row of
                BannerRow _ ->
                    -- Note: This means we're not centering the banner row, we could do that but we'd
                    -- have to figure out the longest row first, and that's a little tricky because
                    -- that might change after justifying the normal rows. So we leave doing the
                    -- centering to when you center the entire table anyway.
                    row

                NormalRow rowContents ->
                    let
                        justifyColumn index ( column, rowCell ) =
                            let
                                longest =
                                    List.getAt index columnSizes
                                        |> Maybe.withDefault 0
                            in
                            Justify.node column.justify longest rowCell
                    in
                    List.zip config.columns rowContents
                        |> List.indexedMap justifyColumn
                        |> NormalRow

        joinRow row =
            case row of
                BannerRow node ->
                    node

                NormalRow rowContents ->
                    rowContents
                        |> List.intersperse (Format.Span [] " ")
                        |> Format.Block
    in
    List.map justifyRow unjustified
        |> List.map joinRow
