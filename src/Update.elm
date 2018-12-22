module Update exposing (update)

import Common exposing (Model, Msg(..))


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1
