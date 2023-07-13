module Tidal.Mini exposing (TidalPattern(..), choose, concatenate, degrade, elongate, euclid, faster, fromString, parser, play, replicate, rest, sequence, slower, stack, toString)

import Parser exposing ((|.), (|=), Parser, Trailing(..))
import Set


type TidalPattern
    = Rest
    | Play String
    | Degrade TidalPattern
    | Faster Float TidalPattern
    | Elongate Int TidalPattern
    | Replicate Int TidalPattern
    | Euclid (List Int) TidalPattern
    | Stack (List TidalPattern)
    | Sequence (List TidalPattern)
    | Concatenate (List TidalPattern)
    | Choose (List TidalPattern)


slower : Float -> TidalPattern -> TidalPattern
slower float =
    Faster (1 / float)


choose : List TidalPattern -> TidalPattern
choose =
    Choose


concatenate : List TidalPattern -> TidalPattern
concatenate =
    Concatenate


sequence : List TidalPattern -> TidalPattern
sequence =
    Sequence


stack : List TidalPattern -> TidalPattern
stack =
    Stack


euclid : { beats : Int, segments : Int, offset : Int } -> TidalPattern -> TidalPattern
euclid args =
    Euclid [ args.beats, args.segments, args.offset ]


replicate : Int -> TidalPattern -> TidalPattern
replicate =
    Replicate


elongate : Int -> TidalPattern -> TidalPattern
elongate =
    Elongate


faster : Float -> TidalPattern -> TidalPattern
faster =
    Faster


degrade : TidalPattern -> TidalPattern
degrade =
    Degrade


play : String -> TidalPattern
play =
    Play


rest : TidalPattern
rest =
    Rest


toString : TidalPattern -> String
toString p0 =
    case p0 of
        Rest ->
            "~"

        Play string ->
            string

        Degrade pattern ->
            toString pattern ++ "?"

        Faster amount pattern ->
            toString pattern
                ++ "*"
                ++ String.fromFloat amount

        Elongate amount pattern ->
            toString pattern
                ++ "@"
                ++ String.fromInt amount

        Replicate amount pattern ->
            toString pattern
                ++ "!"
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

        Concatenate list ->
            "<"
                ++ (list
                        |> List.map toString
                        |> String.join " "
                   )
                ++ ">"


fromString : String -> Maybe TidalPattern
fromString string =
    Parser.run parser string
        |> Result.toMaybe


parser : Parser TidalPattern
parser =
    let
        chooseParser list =
            Parser.oneOf
                [ (Parser.succeed identity
                    |. Parser.symbol "|"
                    |. Parser.spaces
                    |= internalElemParser
                  )
                    |> Parser.andThen (\a -> internalListParser [ a ])
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
                    |= internalElemParser
                  )
                    |> Parser.andThen (\a -> internalListParser [ a ])
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
    in
    internalElemParser
        |> Parser.andThen (\a -> internalListParser [ a ])
        |> Parser.andThen (\a -> chooseParser [ a ])
        |> Parser.andThen (\a -> stackParser [ a ])



-------------------------------------------------------------------
--
--   I N T E R N A L
--
-------------------------------------------------------------------


internalVariableParser =
    Parser.variable
        { start = Char.isAlphaNum
        , inner = Char.isAlphaNum
        , reserved = Set.empty
        }


internalCatParser list =
    Parser.succeed identity
        |= Parser.oneOf
            [ internalElemParser
                |> Parser.andThen (\a -> a :: list |> internalCatParser)
            , Parser.succeed
                (case list of
                    [ a ] ->
                        a

                    _ ->
                        List.reverse list |> Concatenate
                )
            ]


internalElemParser =
    (Parser.succeed identity
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol "["
                |= Parser.lazy (\() -> parser)
                |. Parser.symbol "]"
            , Parser.succeed identity
                |. Parser.symbol "<"
                |= Parser.lazy
                    (\() ->
                        internalElemParser
                            |> Parser.andThen
                                (\a ->
                                    internalCatParser [ a ]
                                )
                    )
                |. Parser.symbol ">"
            , Parser.succeed Rest
                |. Parser.symbol "~"
            , Parser.succeed Play
                |= internalVariableParser
            ]
        |. Parser.spaces
    )
        |> Parser.andThen internalModifierParser


internalModifierParser p =
    Parser.succeed identity
        |= Parser.oneOf
            [ Parser.succeed (Degrade p)
                |. Parser.symbol "?"
                |> Parser.andThen internalModifierParser
            , Parser.succeed (\float -> Faster float p)
                |. Parser.symbol "*"
                |= Parser.float
                |> Parser.andThen internalModifierParser
            , Parser.succeed (\float -> slower float p)
                |. Parser.symbol "/"
                |= Parser.float
                |> Parser.andThen internalModifierParser
            , Parser.succeed (\int -> Elongate int p)
                |. Parser.symbol "@"
                |= Parser.int
                |> Parser.andThen internalModifierParser
            , Parser.succeed (\int -> Replicate int p)
                |. Parser.symbol "!"
                |= Parser.int
                |> Parser.andThen internalModifierParser
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


internalListParser list =
    Parser.succeed identity
        |= Parser.oneOf
            [ internalElemParser
                |> Parser.andThen (\a -> a :: list |> internalListParser)
            , Parser.succeed
                (case list of
                    [ a ] ->
                        a

                    _ ->
                        List.reverse list |> Sequence
                )
            ]
