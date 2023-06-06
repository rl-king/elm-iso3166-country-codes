module Types exposing (Country, Data, Subdivision, subdivisionsDecoder, worldDecoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


type alias Data =
    { world : List Country
    , subdivisions : List Subdivision
    }


type alias Subdivision =
    { country : String
    , code : String
    , name : String
    }


subdivisionsDecoder : Decoder (List Subdivision)
subdivisionsDecoder =
    Decode.list
        (Decode.map3
            (\country code name ->
                { country = country
                , code = code
                , name = name
                }
            )
            (Decode.field "country" Decode.string)
            (Decode.field "code" Decode.string)
            (Decode.field "name" Decode.string)
        )


worldDecoder : Decoder (List Country)
worldDecoder =
    Decode.list countryDecoder
        |> Decode.map (List.sortBy .alpha2)


type alias Country =
    { id : Int
    , alpha2 : String
    , alpha3 : String
    , names : Dict String String
    }


countryDecoder : Decoder Country
countryDecoder =
    Decode.map4
        (\id alpha2 alpha3 all ->
            { id = id
            , alpha2 = alpha2
            , alpha3 = alpha3
            , names =
                all
                    |> List.filterMap
                        (\( k, v ) ->
                            case ( k, Decode.decodeValue Decode.string v ) of
                                ( "id", _ ) ->
                                    Nothing

                                ( "alpha2", _ ) ->
                                    Nothing

                                ( "alpha3", _ ) ->
                                    Nothing

                                ( _, Err _ ) ->
                                    Nothing

                                ( _, Ok sv ) ->
                                    Just ( k, sv )
                        )
                    |> Dict.fromList
            }
        )
        (Decode.field "id" Decode.int)
        (Decode.field "alpha2" Decode.string)
        (Decode.field "alpha3" Decode.string)
        (Decode.keyValuePairs Decode.value)
