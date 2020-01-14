module Spa exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Debug as Debug
import Html exposing (Html, a, div, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)


type alias Model =
    { page : Page, key : Nav.Key }


type Page
    = Gallery
    | Folders
    | SelectedPhoto String
    | NotFound


view : Model -> Document Msg
view model =
    { title = "Photo Groove, SPA Style"
    , body =
        [ div []
            [ lazy viewHeader model.page
            , content model.page
            , viewFooter
            ]
        ]
    }


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Photo Groove" ]

        links =
            ul []
                [ navLink Folders { url = "/", caption = "Folders" }
                , navLink Gallery { url = "/gallery", caption = "Gallery" }
                , navLink (partialStringToPage "/photos/MyPhoto") { url = "/photos/MyPhoto", caption = "See my photo" }
                , navLink (partialStringToPage "/photos/OtherPhotoId") { url = "/photos/OtherPhotoId", caption = "See other photo" }
                ]

        navLink targetPage { url, caption } =
            li [ classList [ ( "active", isActive { page = page, link = targetPage } ) ] ]
                [ a [ href url ] [ text caption ] ]
    in
    nav [] [ logo, links ]


isActive : { link : Page, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ( Gallery, Gallery ) ->
            True

        ( Folders, Folders ) ->
            True

        ( SelectedPhoto pid, SelectedPhoto nid ) ->
            let
                a =
                    Debug.log "foo " ( pid, nid )
            in
            pid == nid

        ( _, _ ) ->
            False


viewFooter : Html msg
viewFooter =
    footer [] [ text "One is never alone with a rubber duck. ‐Douglas Adams" ]


content page =
    case page of
        Gallery ->
            text "Gallery Page"

        Folders ->
            text "Folders PAge"

        SelectedPhoto image ->
            text ("Image of my " ++ image ++ " best interests")

        NotFound ->
            text "Not Found 404"


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url


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
        [ Parser.map Folders Parser.top
        , Parser.map Gallery (s "gallery")
        , Parser.map SelectedPhoto (s "photos" </> Parser.string)
        ]


urlToPage : Url -> Page
urlToPage url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound


partialStringToPage : String -> Page
partialStringToPage str =
    let
        url =
            Url.fromString ("http://dummyDomain.com" ++ str)
    in
    case url of
        Just actualUrl ->
            Parser.parse parser actualUrl
                |> Maybe.withDefault NotFound

        Nothing ->
            NotFound


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { page = urlToPage url, key = key }, Cmd.none )


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
