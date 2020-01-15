port module Spa exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Attribute, Html, a, button, div, h3, img, input, span, text)
import Html.Attributes exposing (autofocus, class, classList, disabled, href, placeholder, src, tabindex, type_, value)
import Html.Events exposing (onClick, onInput)
import Login as Login
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)


port login : Login.Creds -> Cmd msg


port onLoginSuccess : (LoginResponse -> msg) -> Sub msg


port onLoginError : (LoginResponse -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias LoginResponse =
    { foo : String
    }



-- MODEL


type alias Model =
    { page : Page
    , key : Nav.Key
    , creds : Login.Creds
    , loginStatus : LoginStatus
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { page = urlToPage url
      , key = key
      , creds = Login.init
      , loginStatus = Ready
      }
    , Cmd.none
    )


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | LoginMsg Login.Msg
    | OnLogin
    | OnLoginSuccess LoginResponse
    | OnLoginError LoginResponse


type Page
    = Login
    | Register
    | NotFound


type LoginStatus
    = Ready
    | Loading
    | LoginError



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginMsg loginMsg ->
            Login.update loginMsg model.creds
                |> (\( newCred, cmd ) -> ( { model | creds = newCred }, cmd ))

        ChangedUrl url ->
            ( { model | page = urlToPage url }, Cmd.none )

        OnLogin ->
            ( { model | loginStatus = Loading }, login model.creds )

        --Assign login token, Redirect to the root
        OnLoginSuccess res ->
            ( { model | loginStatus = Ready }, Cmd.none )

        OnLoginError res ->
            ( { model | loginStatus = LoginError }, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onLoginSuccess OnLoginSuccess
        , onLoginError OnLoginError
        ]


parser : Parser (Page -> a) a
parser =
    Parser.oneOf
        [ Parser.map Login Parser.top
        , Parser.map Register (s "signup")

        --, Parser.map SelectedPhoto (s "photos" </> Parser.string)
        ]


urlToPage : Url -> Page
urlToPage url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Photo Groove, SPA Style"
    , body =
        [ div []
            [ viewPage model ]
        ]
    }


viewPage model =
    case model.page of
        Login ->
            viewLogin model

        Register ->
            viewRegister model

        NotFound ->
            text "Not Found 404"


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
            , Login.viewEmailAndPassword model.creds |> Html.map LoginMsg
            , button [ class "button flat", classIf isLoading "disabled", disabled isLoading, tabindex 3, onClickIf (not isLoading) OnLogin ] [ span [ classIf isLoading "spinner-border" ] [], text "Log In" ]
            , div [ class "or-label" ] [ text "OR" ]
            , button [ class "button material", tabindex 4 ] [ img [ class "google-icon", src "/icons/google.svg" ] [], text "Log in with Google" ]
            , div [ class "line" ] []
            , div [] [ text "Private testing is taking place. We are sorry that you can't sign up yet" ]
            , div [] [ a [ href "mailto: static.ila@gmail.com" ] [ text "Ask us for an account thought ;)" ] ]
            ]
        ]


viewLoginStatus status =
    case status of
        LoginError ->
            div [ class "login-status" ] [ text "Sorry, but we couldn't find user with such credentials :(" ]

        _ ->
            div [] []


viewRegister model =
    div [ class "form-container" ]
        [ div [ class "form" ]
            [ h3 [] [ text "Sign up to Lean Watch" ]
            , input [ type_ "email", placeholder "Enter email" ] []
            , div [ class "button flat disabled" ] [ text "Continue" ]
            , div [ class "or-label" ] [ text "OR" ]
            , div [ class "button material" ] [ img [ class "google-icon", src "/icons/google.svg" ] [], text "Continue with Google" ]
            , div [ class "line" ] []
            , div [] [ a [ href "/" ] [ text "Already have an account? Log in" ] ]
            ]
        ]


onClickIf : Bool -> Msg -> Attribute Msg
onClickIf condition msg =
    if condition then
        onClick msg

    else
        emptyAttribute


classIf : Bool -> String -> Attribute Msg
classIf condition className =
    if condition then
        class className

    else
        emptyAttribute


emptyAttribute =
    class ""



--Login Page
