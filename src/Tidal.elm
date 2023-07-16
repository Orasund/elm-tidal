module Tidal exposing (TidalCycle, note, sound, toHtml, toString)

import Html exposing (Attribute, Html)
import Html.Attributes
import Tidal.Mini exposing (TidalPattern)
import Url


type TidalCycle
    = Note TidalPattern
    | Sound TidalPattern TidalCycle


note : TidalPattern -> TidalCycle
note =
    Note


sound : TidalPattern -> TidalCycle -> TidalCycle
sound =
    Sound


toString : TidalCycle -> String
toString tidalcycle =
    case tidalcycle of
        Note pattern ->
            "note(\"" ++ Tidal.Mini.toString pattern ++ "\")"

        Sound pattern cycle ->
            (toString cycle ++ "\n")
                ++ "  "
                ++ (".sound(\"" ++ Tidal.Mini.toString pattern ++ "\")")


toHtml : List (Attribute msg) -> TidalCycle -> Html msg
toHtml attrs tidalcycles =
    let
        urlComponent =
            tidalcycles
                |> toString
                |> Url.percentEncode
    in
    Html.iframe
        (("https://strudel.tidalcycles.org/#" ++ urlComponent |> Html.Attributes.src)
            :: attrs
        )
        []
