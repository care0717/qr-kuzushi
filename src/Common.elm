module Common exposing (Model, Msg(..))

import QRCode exposing (ErrorCorrection)


type alias Model =
    { content : String
    , errorCorrection : ErrorCorrection
    }


type Msg
    = Change String
    | Select ErrorCorrection
