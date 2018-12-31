module View exposing (view)

import Common exposing (Model, Msg(..))
import Html exposing (Attribute, Html, div, input, option, select, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (on, onInput, targetValue)
import Json.Decode as Json
import QRCode


onChange : (String -> msg) -> Attribute msg
onChange handler =
    on "change" (Json.map handler targetValue)


qrToDot : String -> Html msg
qrToDot string =
    let
        _ =
            Debug.log "log label" (String.split "\n" string)

        res =
            String.split "\n" string
    in
    div [] [ text string ]


view : Model -> Html Msg
view model =
    let
        handler selectedValue =
            case selectedValue of
                "Low" ->
                    Select QRCode.Low

                "Medium" ->
                    Select QRCode.Medium

                "Quartile" ->
                    Select QRCode.Quartile

                "High" ->
                    Select QRCode.High

                _ ->
                    Select QRCode.Low
    in
    div []
        [ input [ placeholder "Text to reverse", value model.content, onInput Change ] []
        , select [ onChange handler ]
            [ option [ value "Low" ] [ text "Low" ]
            , option [ value "Medium" ] [ text "Medium" ]
            , option [ value "Quartile" ] [ text "Quartile" ]
            , option [ value "High" ] [ text "High" ]
            ]
        , div [] [ QRCode.encodeWith model.errorCorrection model.content |> Result.map QRCode.toSvg |> Result.withDefault (text "Error while encoding to QRCode.") ]
        , QRCode.encodeWith model.errorCorrection model.content |> Result.map QRCode.toString |> Result.withDefault "Error while encoding to QRCode." |> qrToDot
        ]
