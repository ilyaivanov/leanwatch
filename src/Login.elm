port module Login exposing (..)

import Browser.Navigation as Nav
import ExtraEvents exposing (classIf, onClickIf)
import Html exposing (Attribute, Html, a, button, div, h3, img, input, span, text)
import Html.Attributes exposing (autofocus, class, classList, disabled, href, placeholder, src, tabindex, type_, value)
import Html.Events exposing (onInput)


port googleSignin : () -> Cmd msg


type alias Creds =
    { email : String
    , password : String
    }


type Msg
    = SetEmail String
    | SetPassword String
    | OnLoginRequest
    | OnLogin LoginSuccessResponse
    | OnLoginError ()
    | OnLogout String


port onLogin : (LoginSuccessResponse -> msg) -> Sub msg


port onLoginCancel : (() -> msg) -> Sub msg


port onLogout : (String -> msg) -> Sub msg



--MODEL


init : Model
init =
    { email = ""
    , password = ""
    , loginStatus = Anonymous
    }


type alias Model =
    { email : String
    , password : String
    , loginStatus : LoginStatus
    }


type LoginStatus
    = Anonymous
    | LoggedIn LoginSuccessResponse
    | Loading
    | LoginError


type alias LoginSuccessResponse =
    { displayName : String
    , photoURL : String
    , email : String
    }


type alias LoginErrorResponse =
    { message : String
    }



--UPDATE


update : Msg -> Model -> Nav.Key -> ( Model, Cmd msg )
update msg model key =
    case msg of
        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        SetEmail email ->
            ( { model | email = email }, Cmd.none )

        OnLoginRequest ->
            ( { model | loginStatus = Loading }, googleSignin () )

        OnLogin res ->
            ( { model | loginStatus = LoggedIn res }, Nav.pushUrl key "/" )

        OnLoginError _ ->
            ( { model | loginStatus = Anonymous }, Cmd.none )

        OnLogout res ->
            ( { model | loginStatus = Anonymous }, Nav.pushUrl key "/login" )



--VIEW


viewLogin : Model -> Html Msg
viewLogin model =
    let
        isLoading =
            model.loginStatus == Loading
    in
    div [ class "form-container" ]
        [ div [ classList [ ( "form", True ), ( "error", model.loginStatus == LoginError ) ] ]
            [ viewLoginStatus model.loginStatus
            , h3 [] [ text "Log in to Lean Watch" ]
            , input [ type_ "email", tabindex 1, autofocus True, placeholder "Enter email", value model.email, onInput SetEmail ] []
            , input [ type_ "password", tabindex 2, placeholder "Enter password", value model.password, onInput SetPassword ] []
            , button [ class "button flat", classIf isLoading "disabled", disabled isLoading, tabindex 3 ] [ span [ classIf isLoading "spinner-border" ] [], text "Log In" ]
            , div [ class "or-label" ] [ text "OR" ]
            , button [ class "button material", tabindex 4, classIf isLoading "disabled", onClickIf (not isLoading) OnLoginRequest ] [ img [ class "google-icon", src "/icons/google.svg" ] [], text "Log in with Google" ]
            , div [ class "line" ] []
            , div [] [ text "Alpha testing" ]
            ]
        ]


viewLoginStatus status =
    case status of
        LoginError ->
            div [ class "login-status" ] [ text "Sorry, but we couldn't find user with such credentials :(" ]

        _ ->
            div [] []
