module Helpers.Http exposing
    ( Status
    , errorString
    )

import Http


type alias Status a =
    Result Http.Error a


errorString : Http.Error -> String
errorString error =
    case error of
        Http.BadUrl s ->
            String.append "Bad url: " s

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus i ->
            String.fromInt i
                |> String.append "Bad status: "

        Http.BadBody s ->
            String.append "Bad body: " s
