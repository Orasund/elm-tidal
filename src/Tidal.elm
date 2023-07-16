module Tidal exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes
import Tidal.Mini exposing (TidalPattern)
import Url


mini : TidalPattern -> String
mini =
    Tidal.Mini.toString


toHtml : List (Attribute msg) -> String -> Html msg
toHtml attrs string =
    let
        urlComponent =
            Url.percentEncode string
    in
    Html.iframe
        (("https://strudel.tidalcycles.org/#" ++ urlComponent |> Html.Attributes.src)
            :: attrs
        )
        []
