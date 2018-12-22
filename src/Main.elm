module Main exposing (init, main)

import Browser
import Common exposing (Model)
import Update exposing (update)
import View exposing (view)


main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    0
