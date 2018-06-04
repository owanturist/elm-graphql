module GraphQL.Selector
    exposing
        ( Selector
        , Value
        , andThen
        , array
        , bool
        , decodeString
        , decodeValue
        , dict
        , fail
        , field
        , float
        , index
        , int
        , keyValuePairs
        , lazy
        , list
        , map
        , map2
        , map3
        , map4
        , map5
        , map6
        , map7
        , map8
        , maybe
        , null
        , nullable
        , oneOf
        , string
        , succeed
        , value
        )

{-| Turn JSON values into Elm values based on GraphQL response.
Definitely check out this [intro to JSON decoders](https://guide.elm-lang.org/interop/json.html) to get a feel for how this library works!


# Primitives

@docs Selector
@docs string, bool, int, float


# Data Structures

@docs nullable, list, array, dict, keyValuePairs


# Object Primitives

@docs field, index


# Inconsistent Structure

@docs maybe, oneOf


# Run Decoders

@docs Value
@docs decodeString, decodeValue


# Mapping

@docs map, map2, map3, map4, map5, map6, map7, map8


# Fancy Decoding

@docs lazy, value, null, succeed, fail, andThen

-}

import Array exposing (Array)
import Dict exposing (Dict)
import GraphQL.Argument exposing (Argument)
import GraphQL.Internal as Internal
import Json.Decode as Json exposing (Decoder)


{-| -}
type alias Selector a =
    Internal.Selector a


{-| -}
string : Selector String
string =
    Internal.Selector Nothing Json.string


{-| -}
bool : Selector Bool
bool =
    Internal.Selector Nothing Json.bool


{-| -}
int : Selector Int
int =
    Internal.Selector Nothing Json.int


{-| -}
float : Selector Float
float =
    Internal.Selector Nothing Json.float


{-| -}
nullable : Selector a -> Selector (Maybe a)
nullable (Internal.Selector _ decoder) =
    Internal.Selector Nothing (Json.nullable decoder)


{-| -}
list : Selector a -> Selector (List a)
list (Internal.Selector _ decoder) =
    Internal.Selector Nothing (Json.list decoder)


{-| -}
array : Selector a -> Selector (Array a)
array (Internal.Selector _ decoder) =
    Internal.Selector Nothing (Json.array decoder)


{-| -}
dict : Selector a -> Selector (Dict String a)
dict (Internal.Selector _ decoder) =
    Internal.Selector Nothing (Json.dict decoder)


{-| -}
keyValuePairs : Selector a -> Selector (List ( String, a ))
keyValuePairs (Internal.Selector _ decoder) =
    Internal.Selector Nothing (Json.keyValuePairs decoder)


{-| -}
field : String -> List ( String, Argument ) -> Selector a -> Selector (a -> b) -> Selector b
field name arguments (Internal.Selector _ decoder) (Internal.Selector _ fn) =
    Internal.Selector
        (Just
            { name = name
            , alias = Nothing
            , arguments = arguments
            }
        )
        (Json.map2 (|>) decoder fn)


{-| -}
index : Int -> Selector a -> Selector a
index index (Internal.Selector _ decoder) =
    Internal.Selector Nothing (Json.index index decoder)


{-| -}
maybe : Selector a -> Selector (Maybe a)
maybe (Internal.Selector _ decoder) =
    Internal.Selector Nothing (Json.maybe decoder)


{-| -}
oneOf : List (Selector a) -> Selector a
oneOf decoders =
    List.map (\(Internal.Selector _ decoder) -> decoder) decoders
        |> Json.oneOf
        |> Internal.Selector Nothing


{-| -}
type alias Value =
    Json.Value


{-| -}
decodeString : Selector a -> String -> Result String a
decodeString (Internal.Selector _ decoder) string =
    Json.decodeString decoder string


{-| -}
decodeValue : Selector a -> Json.Value -> Result String a
decodeValue (Internal.Selector _ decoder) value =
    Json.decodeValue decoder value


{-| -}
map : (a -> result) -> Selector a -> Selector result
map fn (Internal.Selector _ a) =
    Internal.Selector Nothing (Json.map fn a)


{-| -}
map2 : (a -> b -> result) -> Selector a -> Selector b -> Selector result
map2 fn (Internal.Selector _ a) (Internal.Selector _ b) =
    Internal.Selector Nothing (Json.map2 fn a b)


{-| -}
map3 : (a -> b -> c -> result) -> Selector a -> Selector b -> Selector c -> Selector result
map3 fn (Internal.Selector _ a) (Internal.Selector _ b) (Internal.Selector _ c) =
    Internal.Selector Nothing (Json.map3 fn a b c)


{-| -}
map4 : (a -> b -> c -> d -> result) -> Selector a -> Selector b -> Selector c -> Selector d -> Selector result
map4 fn (Internal.Selector _ a) (Internal.Selector _ b) (Internal.Selector _ c) (Internal.Selector _ d) =
    Internal.Selector Nothing (Json.map4 fn a b c d)


{-| -}
map5 : (a -> b -> c -> d -> e -> result) -> Selector a -> Selector b -> Selector c -> Selector d -> Selector e -> Selector result
map5 fn (Internal.Selector _ a) (Internal.Selector _ b) (Internal.Selector _ c) (Internal.Selector _ d) (Internal.Selector _ e) =
    Internal.Selector Nothing (Json.map5 fn a b c d e)


{-| -}
map6 : (a -> b -> c -> d -> e -> f -> result) -> Selector a -> Selector b -> Selector c -> Selector d -> Selector e -> Selector f -> Selector result
map6 fn (Internal.Selector _ a) (Internal.Selector _ b) (Internal.Selector _ c) (Internal.Selector _ d) (Internal.Selector _ e) (Internal.Selector _ f) =
    Internal.Selector Nothing (Json.map6 fn a b c d e f)


{-| -}
map7 : (a -> b -> c -> d -> e -> f -> g -> result) -> Selector a -> Selector b -> Selector c -> Selector d -> Selector e -> Selector f -> Selector g -> Selector result
map7 fn (Internal.Selector _ a) (Internal.Selector _ b) (Internal.Selector _ c) (Internal.Selector _ d) (Internal.Selector _ e) (Internal.Selector _ f) (Internal.Selector _ g) =
    Internal.Selector Nothing (Json.map7 fn a b c d e f g)


{-| -}
map8 : (a -> b -> c -> d -> e -> f -> g -> h -> result) -> Selector a -> Selector b -> Selector c -> Selector d -> Selector e -> Selector f -> Selector g -> Selector h -> Selector result
map8 fn (Internal.Selector _ a) (Internal.Selector _ b) (Internal.Selector _ c) (Internal.Selector _ d) (Internal.Selector _ e) (Internal.Selector _ f) (Internal.Selector _ g) (Internal.Selector _ h) =
    Internal.Selector Nothing (Json.map8 fn a b c d e f g h)


{-| -}
lazy : (() -> Selector a) -> Selector a
lazy callDecoder =
    callDecoder ()


{-| -}
value : Selector Value
value =
    Internal.Selector Nothing Json.value


{-| -}
null : a -> Selector a
null defaults =
    Internal.Selector Nothing (Json.null defaults)


{-| -}
succeed : a -> Selector a
succeed value =
    Internal.Selector Nothing (Json.succeed value)


{-| -}
fail : String -> Selector a
fail err =
    Internal.Selector Nothing (Json.fail err)


{-| -}
andThen : (a -> Selector result) -> Selector a -> Selector result
andThen fn (Internal.Selector _ decoder) =
    Internal.Selector
        Nothing
        (Json.andThen
            (\a ->
                let
                    (Internal.Selector _ result) =
                        fn a
                in
                result
            )
            decoder
        )
