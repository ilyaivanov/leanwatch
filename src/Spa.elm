module Spa exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, h3, h4, img, input, text)
import Html.Attributes exposing (autofocus, class, href, placeholder, src, tabindex, type_)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { page : Page, key : Nav.Key }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { page = urlToPage url, key = key }, Cmd.none )


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url


type Page
    = Login
    | Register
    | NotFound



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedUrl url ->
            ( { model | page = urlToPage url }, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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


viewLogin model =
    div [ class "form-container" ]
        [ div [ class "form" ]
            [ h3 [] [ text "Log in to Lean Watch" ]
            , input [ type_ "email", tabindex 1, autofocus True, placeholder "Enter email" ] []
            , input [ type_ "password", tabindex 2, placeholder "Enter password" ] []
            , button [ class "button flat", tabindex 3 ] [ text "Log In" ]
            , div [ class "or-label" ] [ text "OR" ]
            , button [ class "button material", tabindex 4 ] [ img [ class "google-icon", src "/icons/google.svg" ] [], text "Log in with Google" ]
            , div [ class "line" ] []
            , div [] [ a [ href "/signup" ] [ text "Sign up for an account" ] ]
            ]
        ]


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



--Login Page
