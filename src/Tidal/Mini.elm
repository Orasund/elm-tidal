module Tidal.Mini exposing (TidalPattern(..), parser, toString)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Set


type TidalPattern
    = Rest
    | Play String
    | Sometimes TidalPattern
    | Faster Float TidalPattern
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

        Faster amount pattern ->
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


parser : Parser TidalPattern
parser =
    let
        variable =
            Parser.variable
                { start = Char.isAlphaNum
                , inner = Char.isAlphaNum
                , reserved = Set.empty
                }

        listParser list =
            Parser.succeed identity
                |= Parser.oneOf
                    [ elemParser
                        |> Parser.andThen (\a -> a :: list |> listParser)
                    , Parser.succeed
                        (case list of
                            [ a ] ->
                                a

                            _ ->
                                List.reverse list |> Sequence
                        )
                    ]

        chooseParser list =
            Parser.oneOf
                [ (Parser.succeed identity
                    |. Parser.symbol "|"
                    |. Parser.spaces
                    |= elemParser
                  )
                    |> Parser.andThen (\a -> listParser [ a ])
                    |> Parser.andThen (\a -> a :: list |> chooseParser)
                , Parser.succeed
                    (case list of
                        [ a ] ->
                            a

                        _ ->
                            List.reverse list |> Choose
                    )
                ]

        stackParser list =
            Parser.oneOf
                [ (Parser.succeed identity
                    |. Parser.symbol ","
                    |. Parser.spaces
                    |= elemParser
                  )
                    |> Parser.andThen (\a -> listParser [ a ])
                    |> Parser.andThen (\a -> chooseParser [ a ])
                    |> Parser.andThen (\a -> a :: list |> stackParser)
                , Parser.succeed
                    (case list of
                        [ a ] ->
                            a

                        _ ->
                            List.reverse list |> Stack
                    )
                ]

        modifier p =
            Parser.succeed identity
                |= Parser.oneOf
                    [ Parser.succeed (Sometimes p)
                        |. Parser.symbol "?"
                        |> Parser.andThen modifier
                    , Parser.succeed (\float -> Faster float p)
                        |. Parser.symbol "*"
                        |= Parser.float
                        |> Parser.andThen modifier
                    , Parser.succeed (\int -> Elongate int p)
                        |. Parser.symbol "@"
                        |= Parser.int
                        |> Parser.andThen modifier
                    , Parser.succeed (\list -> Euclid list p)
                        |= Parser.sequence
                            { start = "("
                            , separator = ","
                            , spaces = Parser.spaces
                            , item = Parser.int
                            , trailing = Forbidden
                            , end = ")"
                            }
                    , Parser.succeed p
                    ]

        elemParser =
            (Parser.succeed identity
                |= Parser.oneOf
                    [ Parser.succeed identity
                        |. Parser.symbol "["
                        |= Parser.lazy (\() -> parser)
                        |. Parser.symbol "]"
                    , Parser.succeed Rest
                        |. Parser.symbol "~"
                    , Parser.succeed Play
                        |= variable
                    ]
                |. Parser.spaces
            )
                |> Parser.andThen modifier
    in
    elemParser
        |> Parser.andThen (\a -> listParser [ a ])
        |> Parser.andThen (\a -> chooseParser [ a ])
        |> Parser.andThen (\a -> stackParser [ a ])
