module Api exposing (..)

import Board exposing (Item, decodeItem)
import Http
import Json.Decode as Json


findVideos msg term =
    Http.get
        { url = "https://europe-west1-lean-watch.cloudfunctions.net/getVideos?q=" ++ term
        , expect = Http.expectJson msg decodeItems
        }


loadNextPageForSearch msg search page =
    Http.get
        { url = "https://europe-west1-lean-watch.cloudfunctions.net/getVideos?q=" ++ search ++ "&pageToken=" ++ page
        , expect = Http.expectJson msg decodeItems
        }


findSimilar msg youtubeId =
    Http.get
        { url = "https://europe-west1-lean-watch.cloudfunctions.net/getVideos?relatedToVideoId=" ++ youtubeId ++ "&type=video"
        , expect = Http.expectJson msg decodeItems
        }


loadNextPageForSimilar msg youtubeId page =
    Http.get
        { url = "https://europe-west1-lean-watch.cloudfunctions.net/getVideos?relatedToVideoId=" ++ youtubeId ++ "&type=video&pageToken=" ++ page
        , expect = Http.expectJson msg decodeItems
        }


type alias SearchResponse =
    { items : List Item
    , nextPageToken : String
    }


decodeItems : Json.Decoder SearchResponse
decodeItems =
    Json.map2 SearchResponse
        (Json.field "items" (Json.list decodeItem))
        (Json.field "nextPageToken" Json.string)
