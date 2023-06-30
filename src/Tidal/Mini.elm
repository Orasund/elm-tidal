module Tidal.Mini exposing (TidalPattern(..), toString)


type TidalPattern
    = Rest
    | Play String
    | Sometimes TidalPattern
    | Fast Float TidalPattern
    | Elongate Int TidalPattern
    | Euclid (List Int) TidalPattern
    | Stack (List TidalPattern)
    | Sequence (List TidalPattern)
    | Choose (List TidalPattern)


toString : TidalPattern -> String
toString p0 =
    case p0 of
        Rest ->
            "~"

        Play string ->
            string

        Sometimes pattern ->
            toString pattern ++ "?"

        Fast amount pattern ->
            toString pattern
                ++ "*"
                ++ String.fromFloat amount

        Elongate amount pattern ->
            toString pattern
                ++ "@"
                ++ String.fromInt amount

        Euclid list pattern ->
            toString pattern
                ++ "("
                ++ (list
                        |> List.map String.fromInt
                        |> String.join ", "
                   )
                ++ ")"

        Stack list ->
            "["
                ++ (list
                        |> List.map toString
                        |> String.join ", "
                   )
                ++ "]"

        Sequence list ->
            "["
                ++ (list
                        |> List.map toString
                        |> String.join " "
                   )
                ++ "]"

        Choose list ->
            "["
                ++ (list
                        |> List.map toString
                        |> String.join " | "
                   )
                ++ "]"
