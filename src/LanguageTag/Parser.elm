module LanguageTag.Parser exposing
    ( parse, ParseResult(..)
    , toLanguageTag
    )

{-| Parse BCP 47 language tags.

@docs parse, ParseResult


# Conversion

@docs toLanguageTag

-}

import LanguageTag exposing (LanguageTag)


{-| A BCP 47 tag can be either

  - a "normal" tag, with language and optional sections for script, region, variants, extensions and private use
  - a "private use" tag, anything beginning with `x-`
  - a "grandfathered" tag, tags used before the standard was created.

-}
type ParseResult
    = Normal
        { language : String
        , script : Maybe String
        , region : Maybe String
        , variants : List String
        , extensions : List String
        , privateUse : List String
        }
    | PrivateUse (List String)
    | Grandfathered (List String)


{-| Parse a BCP47 language tag.
-}
parse : String -> Maybe ParseResult
parse inputString =
    inputString
        |> String.split "-"
        |> parser
        |> Maybe.andThen
            (\( r, leftover ) ->
                if List.isEmpty leftover then
                    Just r

                else
                    Nothing
            )


type alias Parser v =
    List String -> Maybe ( v, List String )


{-| This `Parser` can be used directly instead of through `parse` if you need to combine it with other parsers.
-}
parser : Parser ParseResult
parser =
    -- All parsers pretty much verbatim from RFC5646.
    oneOf
        [ map Grandfathered grandfatheredParser
        , map PrivateUse privateUseParser
        , map Normal langtagParser
        ]


langtagParser :
    Parser
        { language : String
        , script : Maybe String
        , region : Maybe String
        , variants : List String
        , extensions : List String
        , privateUse : List String
        }
langtagParser =
    succeed
        (\language script region variants extensions privateUse ->
            { language = language
            , script = script
            , region = region
            , variants = variants
            , extensions = extensions
            , privateUse = privateUse
            }
        )
        |> keep languageParser
        |> keep (maybe scriptParser)
        |> keep (maybe regionParser)
        |> keep (many variantParser)
        |> keep (many extensionParser)
        |> keep (map (Maybe.withDefault []) (maybe privateUseParser))


languageParser : Parser String
languageParser =
    [ succeed
        (\h t ->
            [ Just h, t ]
                |> List.filterMap identity
                |> String.join "-"
        )
        |> keep (xyAlpha 2 3)
        |> keep (maybe extlangParser)
    , xAlpha 4
    , xyAlpha 5 8
    ]
        |> oneOf


extlangParser : Parser String
extlangParser =
    succeed
        (\h t1 t2 ->
            [ Just h, t1, t2 ]
                |> List.filterMap identity
                |> String.join "-"
        )
        |> keep (xAlpha 3)
        |> keep (maybe (xAlpha 3))
        |> keep (maybe (xAlpha 3))


scriptParser : Parser String
scriptParser =
    xAlpha 4


regionParser : Parser String
regionParser =
    oneOf
        [ xAlpha 2
        , repeat 3 3 Char.isDigit
        ]


variantParser : Parser String
variantParser =
    repeat 4 8 Char.isAlphaNum


extensionParser : Parser String
extensionParser =
    succeed (\h t -> h :: t |> String.join "-")
        |> keep singletonParser
        |> keep
            (some
                (repeat 2 8 Char.isAlphaNum)
            )


singletonParser : Parser String
singletonParser =
    pop <|
        \head ->
            case String.toList head of
                [ c ] ->
                    c /= 'x' && Char.isAlphaNum c

                _ ->
                    False


privateUseParser : Parser (List String)
privateUseParser =
    succeed identity
        |> ignore (symbol "x")
        |> keep (many (repeat 1 8 Char.isAlphaNum))


grandfatheredParser : Parser (List String)
grandfatheredParser =
    oneOf
        [ irregularParser
        , regularParser
        ]


irregularParser : Parser (List String)
irregularParser =
    [ "en-GB-oed"
    , "i-ami"
    , "i-bnn"
    , "i-default"
    , "i-enochian"
    , "i-hak"
    , "i-klingon"
    , "i-lux"
    , "i-mingo"
    , "i-navajo"
    , "i-pwn"
    , "i-tao"
    , "i-tay"
    , "i-tsu"
    , "sgn-BE-FR"
    , "sgn-BE-NL"
    , "sgn-CH-DE"
    ]
        |> List.map
            (\i input ->
                if input == String.split "-" i then
                    Just ( input, [] )

                else
                    Nothing
            )
        |> oneOf


regularParser : Parser (List String)
regularParser =
    [ "art-lojban"
    , "cel-gaulish"
    , "no-bok"
    , "no-nyn"
    , "zh-guoyu"
    , "zh-hakka"
    , "zh-min"
    , "zh-min-nan"
    , "zh-xiang"
    ]
        |> List.map
            (\i input ->
                if input == String.split "-" i then
                    Just ( input, [] )

                else
                    Nothing
            )
        |> oneOf


{-| Convert the result of parsing into a `LanguageTag`.
-}
toLanguageTag : ParseResult -> LanguageTag
toLanguageTag result =
    case result of
        Normal { language, script, region, variants, extensions, privateUse } ->
            (language
                :: Maybe.withDefault [] (Maybe.map List.singleton script)
                ++ Maybe.withDefault [] (Maybe.map List.singleton region)
                ++ variants
                ++ extensions
                ++ (if List.isEmpty privateUse then
                        []

                    else
                        "x" :: privateUse
                   )
            )
                |> String.join "-"
                |> LanguageTag.custom

        PrivateUse privateUse ->
            ("x"
                :: privateUse
            )
                |> String.join "-"
                |> LanguageTag.custom

        Grandfathered grandfathered ->
            grandfathered
                |> String.join "-"
                |> LanguageTag.custom



-- HELPERS --


oneOf : List (Parser a) -> Parser a
oneOf parsers input =
    case parsers of
        [] ->
            Nothing

        head :: tail ->
            case head input of
                Nothing ->
                    oneOf tail input

                Just r ->
                    Just r


{-| Parser zero or one instances, prefixed by "-".
-}
maybe : Parser a -> Parser (Maybe a)
maybe p =
    oneOf
        [ succeed Just
            |> keep p
        , succeed Nothing
        ]


succeed : a -> Parser a
succeed value input =
    Just ( value, input )


map : (a -> b) -> Parser a -> Parser b
map f p input =
    p input
        |> Maybe.map (\( r, l ) -> ( f r, l ))


{-| `xALPHA`, where `x` is an integer.
-}
xAlpha : Int -> Parser String
xAlpha n =
    xyAlpha n n


xyAlpha : Int -> Int -> Parser String
xyAlpha from to =
    repeat from to Char.isAlpha


repeat : Int -> Int -> (Char -> Bool) -> Parser String
repeat from to cond =
    pop <|
        \head ->
            let
                len : Int
                len =
                    String.length head
            in
            (from <= len)
                && (len <= to)
                && List.all cond (String.toList head)


pop : (String -> Bool) -> Parser String
pop check input =
    case input of
        [] ->
            Nothing

        head :: tail ->
            if check head then
                Just ( head, tail )

            else
                Nothing


{-| Parser one or plus instances, prefixed by "-".
-}
some : Parser a -> Parser (List a)
some p =
    succeed (::)
        |> keep p
        |> keep (many p)


keep : Parser a -> Parser (a -> b) -> Parser b
keep second first input =
    first input
        |> Maybe.andThen
            (\( f, tail ) ->
                map f second tail
            )


ignore : Parser ignore -> Parser keep -> Parser keep
ignore second first input =
    first input
        |> Maybe.andThen
            (\( x, tail ) ->
                map (\_ -> x) second tail
            )


{-| Parse zero or plus instances, prefixed by "-".
-}
many : Parser a -> Parser (List a)
many p input =
    case p input of
        Nothing ->
            Just ( [], input )

        Just ( r, tail ) ->
            Maybe.map
                (\( tr, ttail ) -> ( r :: tr, ttail ))
                (many p tail)


symbol : String -> Parser String
symbol s =
    pop <| \head -> head == s
