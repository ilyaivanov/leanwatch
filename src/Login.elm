module Login exposing (..)

import Html exposing (Attribute, Html, div, input)
import Html.Attributes exposing (autofocus, placeholder, tabindex, type_, value)
import Html.Events exposing (onInput)


type Msg
    = SetEmail String
    | SetPassword String


init : Creds
init =
    { email = ""
    , password = ""
    }


type alias Creds =
    { email : String
    , password : String
    }


update : Msg -> Creds -> ( Creds, Cmd msg )
update msg model =
    case msg of
        SetPassword password ->
            ( { email = model.email, password = password }, Cmd.none )

        SetEmail email ->
            ( { email = email, password = model.password }, Cmd.none )


viewEmailAndPassword model =
    div []
        [ input [ type_ "email", tabindex 1, autofocus True, placeholder "Enter email", value model.email, onInput SetEmail ] []
        , input [ type_ "password", tabindex 2, placeholder "Enter password", value model.password, onInput SetPassword ] []
        ]
