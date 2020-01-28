module Main exposing (main)

import Board exposing (BoardResponse, TimeLineInfo, decodeResponse)
import BoardPage as BoardPage
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Attribute, Html, div, text)
import Json.Decode
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



--MODEL


type alias Model =
    { page : Page
    , key : Nav.Key
    , login : Login.Model
    , board : BoardPage.Model
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { page = urlToPage url
      , key = key
      , login = Login.init
      , board = BoardPage.init
      }
    , Cmd.none
    )


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | LoginMsg Login.Msg
    | BoardMsg BoardPage.Msg



--| BoardsLoaded BoardsResponse


type Page
    = Login
    | Boards
    | NotFound



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginMsg loginMsg ->
            Login.update loginMsg model.login model.key
                |> (\( login, cmd ) -> ( { model | login = login }, cmd ))

        BoardMsg boardMsg ->
            BoardPage.update boardMsg model.board
                |> (\( board, cmd ) -> ( { model | board = board }, cmd |> Cmd.map BoardMsg ))

        ChangedUrl url ->
            ( { model | page = urlToPage url }, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )


mapLoadedBoards : Json.Decode.Value -> Msg
mapLoadedBoards modelJson =
    case Json.Decode.decodeValue decodeResponse modelJson of
        Ok model ->
            BoardMsg (BoardPage.BoardsLoaded model)

        Err errorMessage ->
            BoardMsg BoardPage.Noop


mapVideoProgress : Json.Decode.Value -> Msg
mapVideoProgress json =
    let
        decoder =
            Json.Decode.map2 TimeLineInfo
                (Json.Decode.field "duration" Json.Decode.float)
                (Json.Decode.field "currentTime" Json.Decode.float)
    in
    case Json.Decode.decodeValue decoder json of
        Ok model ->
            BoardMsg (BoardPage.UpdateTimeline model)

        Err errorMessage ->
            BoardMsg BoardPage.Noop


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Login.onLogin Login.OnLogin |> Sub.map LoginMsg
        , Login.onLogout Login.OnLogout |> Sub.map LoginMsg
        , Login.onLoginCancel Login.OnLoginError |> Sub.map LoginMsg
        , BoardPage.onUserProfileLoaded BoardPage.UserProfileLoaded |> Sub.map BoardMsg
        , BoardPage.onBoardCreated BoardPage.OnBoardCreated |> Sub.map BoardMsg
        , BoardPage.onVideoEnded BoardPage.VideoEnded |> Sub.map BoardMsg
        , BoardPage.onBoardsLoaded mapLoadedBoards
        , BoardPage.onVideoProgress mapVideoProgress
        ]


parser : Parser (Page -> a) a
parser =
    Parser.oneOf
        [ Parser.map Login (s "login")
        , Parser.map Boards Parser.top
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
            let
                loginStatus =
                    case model.login.loginStatus of
                        Login.LoggedIn status ->
                            Just status

                        _ ->
                            Nothing
            in
            BoardPage.view model.board loginStatus |> Html.map BoardMsg

        NotFound ->
            text "NotFound"
