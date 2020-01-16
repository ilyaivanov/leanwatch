port module Main exposing (main)

import Board as Board
import Browser exposing (Document)
import Browser.Navigation as Nav
import Dict exposing (Dict)
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


port loadBoards : String -> Cmd msg


port onBoardsResponse : (BoardsResponse -> msg) -> Sub msg



-- NORMALIZATION to extract


type alias BoardsResponse =
    { selectedBoard : String
    , boards :
        List
            { id : String
            , name : String
            , stacks :
                List
                    { id : String
                    , name : String
                    , items :
                        List
                            { id : String
                            , name : String
                            , youtubeId : String
                            }
                    }
            }
    }


createBoardsModel : BoardsResponse -> Board.Model
createBoardsModel boardResponse =
    let
        model =
            Board.init

        allBoards =
            boardResponse.boards

        allStacks =
            List.map (\b -> b.stacks) allBoards |> List.concat

        allItems =
            List.map (\s -> s.items) allStacks |> List.concat

        getIds =
            List.map (\s -> s.id)
    in
    { model
        | boards = Dict.fromList (List.map (\b -> ( b.id, Board.Board b.id b.name (getIds b.stacks) )) allBoards)
        , stacks = Dict.fromList (List.map (\s -> ( s.id, Board.Stack s.id s.name (getIds s.items) )) allStacks)
        , items = Dict.fromList (List.map (\i -> ( i.id, Board.Item i.id i.name i.youtubeId )) allItems)
        , boardsOrder = getIds allBoards
        , selectedBoard = boardResponse.selectedBoard
    }



-- MODEL


type alias Model =
    { page : Page
    , key : Nav.Key
    , login : Login.Model
    , board : Board.Model
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { page = urlToPage url
      , key = key
      , login = Login.init
      , board = Board.init
      }
    , Cmd.none
    )


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | LoginMsg Login.Msg
    | BoardMsg Board.Msg
    | BoardsLoaded BoardsResponse


type Page
    = Login
    | Boards
    | NotFound



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginMsg loginMsg ->
            case loginMsg of
                Login.OnLoginSuccess res ->
                    let
                        nextLoginModel =
                            Login.update (Login.OnLoginSuccess res) model.login |> Tuple.first

                        goToRoot =
                            Nav.pushUrl model.key "/"

                        loadBoardsAction =
                            loadBoards res.token
                    in
                    ( { model | login = nextLoginModel }, Cmd.batch [ goToRoot, loadBoardsAction ] )

                _ ->
                    Login.update loginMsg model.login
                        |> (\( login, cmd ) -> ( { model | login = login }, cmd ))

        BoardMsg boardMsg ->
            Board.update boardMsg model.board
                |> (\( board, cmd ) -> ( { model | board = board }, cmd |> Cmd.map BoardMsg ))

        BoardsLoaded boards ->
            ( { model | board = createBoardsModel boards }, Cmd.none )

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
        [ Login.onLoginSuccess Login.OnLoginSuccess |> Sub.map LoginMsg
        , Login.onLoginError Login.OnLoginError |> Sub.map LoginMsg
        , onBoardsResponse BoardsLoaded
        ]


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
            Board.view model.board |> Html.map BoardMsg

        NotFound ->
            text "NotFound"
