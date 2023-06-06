module Generate exposing (run)

{-

   Generate Elm files

   Data provided by https://github.com/stefangabos/world_countries

-}

import BackendTask exposing (BackendTask)
import BackendTask.Http as Http
import Dict
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)
import Set exposing (Set)
import Types exposing (Country, Data, subdivisionsDecoder, worldDecoder)


run : Script
run =
    BackendTask.succeed
        (\world subdivisions ->
            { world = world, subdivisions = subdivisions }
        )
        |> ignore (\_ -> Script.log "Getting countries' and subdivisions' data")
        |> BackendTask.andMap
            (Http.getJson
                "https://raw.githubusercontent.com/stefangabos/world_countries/master/data/countries/_combined/world.json"
                worldDecoder
                |> BackendTask.allowFatal
            )
        |> BackendTask.andMap
            (Http.getJson
                "https://raw.githubusercontent.com/stefangabos/world_countries/master/data/subdivisions/subdivisions.json"
                subdivisionsDecoder
                |> BackendTask.allowFatal
            )
        |> ignore
            (\{ world, subdivisions } ->
                Script.log <|
                    "Downloaded "
                        ++ String.fromInt (List.length world)
                        ++ " countries and "
                        ++ String.fromInt (List.length subdivisions)
                        ++ " subdivisions"
            )
        |> ignore checkCompleteness
        |> BackendTask.andThen generate
        |> Script.withoutCliOptions


checkCompleteness : Data -> BackendTask FatalError ()
checkCompleteness { world } =
    let
        worldLanguages : Set String
        worldLanguages =
            world
                |> List.foldl
                    (\country ->
                        Set.union (Set.fromList <| Dict.keys country.names)
                    )
                    Set.empty

        mappedLanguages : Set String
        mappedLanguages =
            Set.fromList (List.map (\( alpha2, _, _ ) -> alpha2) languages)

        missing : Set String
        missing =
            Set.diff worldLanguages mappedLanguages
    in
    if Set.isEmpty missing then
        BackendTask.succeed ()

    else
        BackendTask.fail
            (FatalError.fromString <|
                "Missing languages: "
                    ++ String.join ", " (Set.toList missing)
            )


ignore : (a -> BackendTask error x) -> BackendTask error a -> BackendTask error a
ignore ignored after =
    BackendTask.andThen (\x -> BackendTask.map (\_ -> x) (ignored x)) after


languages : List ( String, String, List String )
languages =
    [ ( "ar", "Arabic", [ "Arabic" ] )
    , ( "bg", "Bulgarian", [ "Bulgarian" ] )
    , ( "cs", "Czech", [ "Czech" ] )
    , ( "da", "Danish", [ "Danish" ] )
    , ( "de", "German", [ "German" ] )
    , ( "el", "Greek", [ "Greek" ] )
    , ( "en", "English", [ "English" ] )
    , ( "eo", "Esperanto", [ "Esperanto" ] )
    , ( "es", "Spanish", [ "Spanish" ] )
    , ( "et", "Estonian", [ "Estonian" ] )
    , ( "eu", "Basque", [ "Basque" ] )
    , ( "fi", "Finnish", [ "Finnish" ] )
    , ( "fr", "French", [ "French" ] )
    , ( "hr", "Croatian", [ "Croatian" ] )
    , ( "hu", "Hungarian", [ "Hungarian" ] )
    , ( "hy", "Armenian", [ "Armenian" ] )
    , ( "it", "Italian", [ "Italian" ] )
    , ( "ja", "Japanese", [ "Japanese" ] )
    , ( "ko", "Korean", [ "Korean" ] )
    , ( "lt", "Lithuanian", [ "Lithuanian" ] )
    , ( "nl", "Dutch", [ "Dutch" ] )
    , ( "no", "Norwegian", [ "Norwegian" ] )
    , ( "pl", "Polish", [ "Polish" ] )
    , ( "pt", "Portuguese", [ "Portuguese" ] )
    , ( "ro", "Romanian", [ "Romanian" ] )
    , ( "ru", "Russian", [ "Russian" ] )
    , ( "sk", "Slovak", [ "Slovak" ] )
    , ( "sl", "Slovenian", [ "Slovenian" ] )
    , ( "sv", "Sweden", [ "Sweden" ] )
    , ( "sr", "Serbian", [ "Serbian" ] )
    , ( "th", "Thai", [ "Thai" ] )
    , ( "uk", "Ukrainian", [ "Ukrainian" ] )
    , ( "zh", "Chinese (Simplified)", [ "Chinese" ] )
    , ( "zh-tw", "Chinese (Traditional)", [ "Chinese", "Traditional" ] )
    ]


generate : Data -> BackendTask FatalError ()
generate data =
    (generateMainModule data
        :: List.map
            (\( k, name, moduleName ) ->
                generateForLanguage k name moduleName data
            )
            languages
    )
        |> BackendTask.combine
        |> BackendTask.map (\_ -> ())


generateMainModule : Data -> BackendTask FatalError ()
generateMainModule data =
    Script.writeFile
        { path =
            [ "src", "Iso3166.elm" ]
                |> String.join "/"
        , body =
            mainModule data
                |> String.join "\n"
        }
        |> BackendTask.allowFatal


mainModule : Data -> List String
mainModule data =
    let
        countryCodes : List String
        countryCodes =
            List.map toCountryCode data.world
    in
    [ "module Iso3166 exposing (CountryCode(..),toAlpha2,fromAlpha2,toAlpha3,fromAlpha3,toNumeric,fromNumeric,all)"
    , ""
    , "-- Generated by 'generate/Main.hs' do not edit by hand"
    , ""
    , "{-| Convert to and from Iso3166 country codes."
    , "@docs toAlpha2"
    , "@docs fromAlpha2"
    , "@docs toAlpha3"
    , "@docs fromAlpha3"
    , "@docs toNumeric"
    , "@docs fromNumeric"
    , "@docs all"
    , "# Definition"
    , "@docs CountryCode"
    , "-}"
    , ""
    , "{-| `GT` and `LT` are defined in `Basics` so we define them as `GT_` and `LT_`."
    , "-}"
    , "type CountryCode"
    , "    = " ++ String.join "\n    | " countryCodes
    , ""
    , "{-| Two-letter `ISO 3166-1 alpha-2` code from `CountryCode`."
    , "-}"
    , "toAlpha2 : CountryCode -> String"
    , "toAlpha2 c ="
    , "    case c of"
    , data.world
        |> List.map (\country -> "        " ++ toCountryCode country ++ " -> \"" ++ country.alpha2 ++ "\"")
        |> String.join "\n"
    , ""
    , "{-| `CountryCode` from two-letter `ISO 3166-1 alpha-2` code."
    , "-}"
    , "fromAlpha2 : String -> Maybe CountryCode"
    , "fromAlpha2 s ="
    , "    case s of"
    , data.world
        |> List.map (\country -> "        \"" ++ country.alpha2 ++ "\" -> Just " ++ toCountryCode country)
        |> String.join "\n"
    , "        _ -> Nothing"
    , ""
    , "{-| Three-letter `ISO 3166-1 alpha-3` code from `CountryCode`."
    , "-}"
    , "toAlpha3 : CountryCode -> String"
    , "toAlpha3 c ="
    , "    case c of"
    , data.world
        |> List.map (\country -> "        " ++ toCountryCode country ++ " -> \"" ++ country.alpha3 ++ "\"")
        |> String.join "\n"
    , ""
    , "{-| `CountryCode` from three-letter `ISO 3166-1 alpha-3` code."
    , "-}"
    , "fromAlpha3 : String -> Maybe CountryCode"
    , "fromAlpha3 s ="
    , "    case s of"
    , data.world
        |> List.map (\country -> "        \"" ++ country.alpha3 ++ "\" -> Just " ++ toCountryCode country)
        |> String.join "\n"
    , "        _ -> Nothing"
    , ""
    , "{-| `ISO 3166-1 numeric` code from `CountryCode`."
    , "-}"
    , "toNumeric : CountryCode -> Int"
    , "toNumeric c ="
    , "    case c of"
    , data.world
        |> List.map (\country -> "        " ++ toCountryCode country ++ " -> " ++ String.fromInt country.id)
        |> String.join "\n"
    , ""
    , "{-| `CountryCode` from `ISO 3166-1 numeric` code."
    , "-}"
    , "fromNumeric : Int -> Maybe CountryCode"
    , "fromNumeric i ="
    , "    case i of"
    , data.world
        |> List.map (\country -> "        " ++ String.fromInt country.id ++ " -> Just " ++ toCountryCode country)
        |> String.join "\n"
    , "        _ -> Nothing"
    , ""
    , "{-| All `CountryCode`s sorted alphabetically."
    , "-}"
    , "all : List CountryCode"
    , "all  ="
    , "    [ " ++ String.join "," countryCodes ++ " ]"
    ]


generateForLanguage : String -> String -> List String -> Data -> BackendTask FatalError ()
generateForLanguage alpha2 languageName moduleName data =
    Script.writeFile
        { path =
            "src/Iso3166/" ++ String.join "/" moduleName ++ ".elm"
        , body =
            languageModule alpha2 languageName moduleName data
                |> String.join "\n"
        }
        |> BackendTask.allowFatal


languageModule : String -> String -> List String -> Data -> List String
languageModule alpha2 languageName moduleName data =
    let
        list : List ( String, String )
        list =
            List.filterMap
                (\country ->
                    Dict.get alpha2 country.names
                        |> Maybe.map
                            (\name ->
                                ( toCountryCode country, "\"" ++ name ++ "\"" )
                            )
                )
                data.world

        docList : String
        docList =
            list
                |> List.map (\( k, v ) -> k ++ " " ++ v)
                |> String.join "\n"
    in
    [ "module Iso3166." ++ String.join "." moduleName ++ " exposing (toName)"
    , ""
    , "-- Generated by 'generate/Main.hs' do not edit by hand"
    , ""
    , "{-|"
    , "@docs toName"
    , "-}"
    , ""
    , "import Iso3166 exposing (CountryCode(..))"
    , ""
    , "{-| Name for `CountryCode` in " ++ languageName ++ "."
    , ""
    , "```"
    , docList
    , "```"
    , "-}"
    , "toName : CountryCode -> String"
    , "toName c ="
    , "    case c of"
    , list
        |> List.map (\( k, v ) -> "        " ++ k ++ " -> " ++ v)
        |> String.join "\n"
    ]


toCountryCode : Country -> String
toCountryCode { alpha2 } =
    case alpha2 of
        "gt" ->
            "GT_"

        "lt" ->
            "LT_"

        _ ->
            String.toUpper alpha2
