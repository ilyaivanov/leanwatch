module Spa exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Attribute, Html, div, text)
import Login as Login
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)


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



-- MODEL


type alias Model =
    { page : Page
    , key : Nav.Key
    , login : Login.Model
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { page = urlToPage url
      , key = key
      , login = Login.init
      }
    , Cmd.none
    )


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | LoginMsg Login.Msg


type Page
    = Login
    | Boards
    | NotFound



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginMsg loginMsg ->
            Login.update loginMsg model.login
                |> (\( login, cmd ) -> ( { model | login = login }, cmd ))

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
    Sub.batch
        [ Login.onLoginSuccess Login.OnLoginSuccess
        , Login.onLoginError Login.OnLoginError
        ]
        |> Sub.map LoginMsg


parser : Parser (Page -> a) a
parser =
    Parser.oneOf
        [ Parser.map Login (s "login")
        , Parser.map Boards Parser.top

        --, Parser.map SelectedPhoto (s "photos" </> Parser.string)
        ]


urlToPage : Url -> Page
urlToPage url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Lean Watch"
    , body =
        [ div []
            [ viewPage model ]
        ]
    }


viewPage model =
    case model.page of
        Login ->
            Login.viewLogin model.login |> Html.map LoginMsg

        Boards ->
            text "Boards go to /login manually"

        NotFound ->
            text "NotFound"
