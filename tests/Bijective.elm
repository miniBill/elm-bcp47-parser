module Bijective exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import LanguageTag
import LanguageTag.Parser exposing (ParseResult)
import Test exposing (Test)


suite : Test
suite =
    Test.fuzz
        (Fuzz.list componentFuzzer)
        "parse >> toString === identity (fuzz)"
        (\input -> isBijectiveOn (String.join "-" input))


isBijectiveOn : String -> Expect.Expectation
isBijectiveOn inputString =
    let
        parsed : Maybe ParseResult
        parsed =
            LanguageTag.Parser.parse inputString
    in
    case parsed of
        Nothing ->
            Expect.pass

        Just result ->
            LanguageTag.Parser.toLanguageTag result
                |> LanguageTag.toString
                |> Expect.equal inputString


componentFuzzer : Fuzzer String
componentFuzzer =
    Fuzz.string
