module View exposing (view)

import Common exposing (Model, Msg(..))
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
