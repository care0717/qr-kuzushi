module Update exposing (update)

import Common exposing (Model, Msg(..))


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }

        Select ec ->
            { model | errorCorrection = ec }
