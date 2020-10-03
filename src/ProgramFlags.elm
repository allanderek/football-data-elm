module ProgramFlags exposing
    ( ProgramFlags
    , decodeFlag
    )

import Json.Decode as Decode exposing (Decoder)


type alias ProgramFlags =
    Decode.Value


decodeFlag : ProgramFlags -> a -> String -> Decoder a -> a
decodeFlag programFlags default fieldName fieldDecoder =
    Decode.decodeValue (Decode.field fieldName fieldDecoder) programFlags
        |> Result.withDefault default
