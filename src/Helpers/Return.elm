module Helpers.Return exposing
    ( noCommand
    , withCommand
    , withCommands
    )


withCommand : Cmd msg -> model -> ( model, Cmd msg )
withCommand command model =
    ( model, command )


withCommands : List (Cmd msg) -> model -> ( model, Cmd msg )
withCommands commands model =
    ( model, Cmd.batch commands )


noCommand : model -> ( model, Cmd msg )
noCommand model =
    ( model, Cmd.none )
