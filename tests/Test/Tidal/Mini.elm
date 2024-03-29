module Test.Tidal.Mini exposing (..)

import Expect
import Parser
import Test exposing (..)
import Tidal.Mini exposing (TidalPattern(..))


parsingExpect : String -> String -> Test
parsingExpect out string =
    test (string ++ " to " ++ out)
        (\() ->
            Parser.run Tidal.Mini.parser string
                |> Result.map Tidal.Mini.toString
                |> Expect.equal (Ok out)
        )


suite : Test
suite =
    describe "basics"
        [ "[~]" |> parsingExpect "~"
        , "[d4]?" |> parsingExpect "d4?"
        , "[d4?]?" |> parsingExpect "d4??"
        , "d4?*2!4" |> parsingExpect "d4?*2!4"
        , "d4*2.5?" |> parsingExpect "d4*2.5?"
        , "d4/2*2" |> parsingExpect "d4*0.5*2"
        , "d4@2" |> parsingExpect "d4@2"
        , "d4(1,2,3)" |> parsingExpect "d4(1, 2, 3)"
        , "[ [[c1] d1 ] e1 ]" |> parsingExpect "[[c1 d1] e1]"
        , "c1 | d1" |> parsingExpect "[c1 | d1]"
        , "c1 | d1 e1 | c1" |> parsingExpect "[c1 | [d1 e1] | c1]"
        , "c1 , d1 e1" |> parsingExpect "[c1, [d1 e1]]"
        , "c1 [e1 | d1 , f1 g1], a1" |> parsingExpect "[[c1 [[e1 | d1], [f1 g1]]], a1]"
        , "< d1 c1 e1 >" |> parsingExpect "<d1 c1 e1>"
        , "{ c1, d2 e3, {e f}}" |> parsingExpect "{[c1, [d2 e3], {[e f]}]}"
        ]
