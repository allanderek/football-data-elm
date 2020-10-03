module Helpers.Decode exposing
    ( andField
    , andFieldAt
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode


andField : String -> Decoder a -> Decoder (a -> b) -> Decoder b
andField fieldName valueDecoder =
    Decode.andMap (Decode.field fieldName valueDecoder)


andFieldAt : List String -> Decoder a -> Decoder (a -> b) -> Decoder b
andFieldAt fieldPath valueDecoder =
    Decode.andMap (Decode.at fieldPath valueDecoder)
