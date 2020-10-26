module Helpers.Return exposing
    ( addCommand
    , combine
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


addCommand : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
addCommand command ( model, alreadyCommand ) =
    ( model, Cmd.batch [ alreadyCommand, command ] )


combine : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
combine update ( model, command ) =
    let
        ( newModel, extraCommand ) =
            update model
    in
    ( newModel, Cmd.batch [ command, extraCommand ] )

