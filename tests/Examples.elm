module Examples exposing (suite)

import Expect exposing (Expectation)
import LanguageTag
import LanguageTag.Parser exposing (ParseResult)
import Test exposing (Test)


suite : Test
suite =
    (examples ++ invalid)
        |> List.map (\input -> Test.test input <| \_ -> isBijectiveOn input)
        |> Test.describe "parse >> toString === identity (examples)"


isBijectiveOn : String -> Expectation
isBijectiveOn inputString =
    let
        parsed : Maybe ParseResult
        parsed =
            LanguageTag.Parser.parse inputString
    in
    case parsed of
        Nothing ->
            Expect.fail "Failed to parse"

        Just result ->
            LanguageTag.Parser.toLanguageTag result
                |> LanguageTag.toString
                |> Expect.equal inputString


examples : List String
examples =
    [ "de"
    , "fr"
    , "ja"
    , "i-enochian"
    , "zh-Hant"
    , "zh-Hans"
    , "sr-Cyrl"
    , "sr-Latn"
    , "zh-cmn-Hans-CN"
    , "cmn-Hans-CN"
    , "zh-yue-HK"
    , "yue-HK"
    , "zh-Hans-CN"
    , "sr-Latn-RS"
    , "sl-rozaj"
    , "sl-rozaj-biske"
    , "sl-nedis"
    , "de-CH-1901"
    , "sl-IT-nedis"
    , "hy-Latn-IT-arevela"
    , "de-DE"
    , "en-US"
    , "es-419"
    , "de-CH-x-phonebk"
    , "az-Arab-x-AZE-derbend"
    , "x-whatever"
    , "qaa-Qaaa-QM-x-southern"
    , "de-Qaaa"
    , "sr-Latn-QM"
    , "sr-Qaaa-RS"
    , "en-US-u-islamcal"
    , "zh-CN-a-myext-x-private"
    , "en-a-myext-b-another"
    ]


invalid : List String
invalid =
    [ -- "de-419-DE",
      -- "a-DE",
      "ar-a-aaa-b-bbb-a-ccc"
    ]
