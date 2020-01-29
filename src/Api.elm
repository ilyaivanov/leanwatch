module Api exposing (..)

import Board exposing (Item, decodeItem)
import Http
import Json.Decode as Json


base =
    "https://europe-west1-lean-watch.cloudfunctions.net/"



--base =
--    "http://localhost:5000/lean-watch/us-central1/"


findVideos msg term =
    Http.get
        { url = base ++ "getVideos?q=" ++ term
        , expect = Http.expectJson msg decodeItems
        }


loadNextPageForSearch msg search page =
    Http.get
        { url = base ++ "getVideos?q=" ++ search ++ "&pageToken=" ++ page
        , expect = Http.expectJson msg decodeItems
        }


findSimilar msg youtubeId =
    Http.get
        { url = base ++ "getVideos?relatedToVideoId=" ++ youtubeId ++ "&type=video"
        , expect = Http.expectJson msg decodeItems
        }


loadNextPageForSimilar msg youtubeId page =
    Http.get
        { url = base ++ "getVideos?relatedToVideoId=" ++ youtubeId ++ "&type=video&pageToken=" ++ page
        , expect = Http.expectJson msg decodeItems
        }


loadPlaylist msg playlistId =
    Http.get
        { url = base ++ "getPlaylistItems?playlistId=" ++ playlistId
        , expect = Http.expectJson msg decodeItems
        }


type alias SearchResponse =
    { items : List Item
    , nextPageToken : Maybe String
    }


decodeItems : Json.Decoder SearchResponse
decodeItems =
    Json.map2 SearchResponse
        (Json.field "items" (Json.list decodeItem))
        (Json.maybe (Json.field "nextPageToken" Json.string))
