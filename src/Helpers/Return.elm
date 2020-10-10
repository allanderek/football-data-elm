module Helpers.Return exposing
    ( combine
    , noCommand
    , withCommand
    , withCommands
    , withModel
    )


withModel : model -> Cmd msg -> ( model, Cmd msg )
withModel model command =
    ( model, command )


withCommand : Cmd msg -> model -> ( model, Cmd msg )
withCommand command model =
    ( model, command )


withCommands : List (Cmd msg) -> model -> ( model, Cmd msg )
withCommands commands model =
    ( model, Cmd.batch commands )


noCommand : model -> ( model, Cmd msg )
noCommand model =
    ( model, Cmd.none )


combine : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
combine update ( model, command ) =
    let
        ( newModel, extraCommand ) =
            update model
    in
    ( model, Cmd.batch [ command, extraCommand ] )
