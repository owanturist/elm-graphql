module GraphQL.Selector
    exposing
        ( Selector
        , aliased
        , array
        , bool
        , decodeString
        , decodeValue
        , dict
        , field
        , float
        , index
        , int
        , keyValuePairs
        , list
        , maybe
        , nullable
        , render
        , string
        , succeed
        )

{-| Build GraphQL with decoders for turning JSON values into Elm values.


# Primitives

@docs Selector
@docs string, bool, int, float


# Data Structures

@docs nullable, list, array, dict, keyValuePairs


# Object Primitives

@docs field, aliased, index


# Inconsistent Structure

@docs maybe


# Run Selectors

@docs render, decodeString, decodeValue


# Fancy Decoding

@docs succeed

-}

import Array exposing (Array)
import Dict exposing (Dict)
import GraphQL.Internal as Internal exposing (Argument)
import Json.Decode as Json exposing (Decoder)


{-| A value that knows how to select (receive and decode) a GraphQL values.
-}
type Selector a
    = Selector (Maybe String) (Decoder a)


{-| Decode a JSON string into an Elm `String`.

    decodeString string "true"              == Err ...
    decodeString string "42"                == Err ...
    decodeString string "3.14"              == Err ...
    decodeString string "\"hello\""         == Ok "hello"
    decodeString string "{ \"hello\": 42 }" == Err ...

-}
string : Selector String
string =
    Selector Nothing Json.string


{-| Decode a JSON string into an Elm `Bool`.

    decodeString bool "true"              == Ok True
    decodeString bool "42"                == Err ...
    decodeString bool "3.14"              == Err ...
    decodeString bool "\"hello\""         == Err ...
    decodeString bool "{ \"hello\": 42 }" == Err ...

-}
bool : Selector Bool
bool =
    Selector Nothing Json.bool


{-| Decode a JSON number into an Elm `Int`.

    decodeString int "true"              == Err ...
    decodeString int "42"                == Ok 42
    decodeString int "3.14"              == Err ...
    decodeString int "\"hello\""         == Err ...
    decodeString int "{ \"hello\": 42 }" == Err ...

-}
int : Selector Int
int =
    Selector Nothing Json.int


{-| Decode a JSON number into an Elm `Float`.

    decodeString float "true"              == Err ..
    decodeString float "42"                == Ok 42
    decodeString float "3.14"              == Ok 3.14
    decodeString float "\"hello\""         == Err ...
    decodeString float "{ \"hello\": 42 }" == Err ...

-}
float : Selector Float
float =
    Selector Nothing Json.float


{-| Decode a nullable JSON value into an Elm value.

    decodeString (nullable int) "13"    == Ok (Just 13)
    decodeString (nullable int) "42"    == Ok (Just 42)
    decodeString (nullable int) "null"  == Ok Nothing
    decodeString (nullable int) "true"  == Err ...

-}
nullable : Selector a -> Selector (Maybe a)
nullable (Selector query decoder) =
    Selector query (Json.nullable decoder)


{-| Decode a JSON array into an Elm `List`.

    decodeString (list int) "[1,2,3]"       == Ok [1,2,3]
    decodeString (list bool) "[true,false]" == Ok [True,False]

-}
list : Selector a -> Selector (List a)
list (Selector query decoder) =
    Selector query (Json.list decoder)


{-| Decode a JSON array into an Elm `Array`.

    decodeString (array int) "[1,2,3]"       == Ok (Array.fromList [1,2,3])
    decodeString (array bool) "[true,false]" == Ok (Array.fromList [True,False])

-}
array : Selector a -> Selector (Array a)
array (Selector query decoder) =
    Selector query (Json.array decoder)


{-| Decode a JSON object into an Elm `Dict`.

    decodeString (dict int) "{ \"alice\": 42, \"bob\": 99 }"
      == Dict.fromList [("alice", 42), ("bob", 99)]

-}
dict : Selector a -> Selector (Dict String a)
dict (Selector query decoder) =
    Selector query (Json.dict decoder)


{-| Decode a JSON object into an Elm `List` of pairs.

    decodeString (keyValuePairs int) "{ \"alice\": 42, \"bob\": 99 }"
      == [("alice", 42), ("bob", 99)]

-}
keyValuePairs : Selector a -> Selector (List ( String, a ))
keyValuePairs (Selector query decoder) =
    Selector query (Json.keyValuePairs decoder)


selector : Maybe String -> String -> List ( String, Argument ) -> Selector a -> Selector (a -> b) -> Selector b
selector alias name arguments (Selector query1 decoder) (Selector query2 next) =
    let
        fieldDecoder =
            Json.field (Maybe.withDefault name alias) decoder

        fieldOrAlias =
            case alias of
                Nothing ->
                    name

                Just alias ->
                    alias ++ ":" ++ name

        args =
            Internal.renderArguments arguments
                |> Maybe.map (Internal.wrap "(" ")")
                |> Maybe.withDefault ""

        query =
            case ( query1, query2 ) of
                ( Nothing, Nothing ) ->
                    fieldOrAlias ++ args

                ( Nothing, Just prev ) ->
                    prev ++ " " ++ fieldOrAlias ++ args

                ( Just child, Nothing ) ->
                    fieldOrAlias ++ args ++ Internal.wrap "{" "}" child

                ( Just child, Just prev ) ->
                    prev ++ " " ++ fieldOrAlias ++ args ++ Internal.wrap "{" "}" child
    in
    Selector (Just query) (Json.map2 (|>) fieldDecoder next)


{-| -}
field : String -> List ( String, Argument ) -> Selector a -> Selector (a -> b) -> Selector b
field =
    selector Nothing


{-| -}
aliased : String -> String -> List ( String, Argument ) -> Selector a -> Selector (a -> b) -> Selector b
aliased =
    selector << Just


{-| Decode a JSON array, requiring a particular index.

    json = """[ "alice", "bob", "chuck" ]"""

    decodeString (index 0 string) json  == Ok "alice"
    decodeString (index 1 string) json  == Ok "bob"
    decodeString (index 2 string) json  == Ok "chuck"
    decodeString (index 3 string) json  == Err ...

-}
index : Int -> Selector a -> Selector a
index i (Selector query decoder) =
    Selector query (Json.index i decoder)


{-| Helpful for dealing with optional fields. Here are a few slightly different examples:

    json = """{ "name": "tom", "age": 42 }"""

    decodeString (succeed identity |> field "age" [] (maybe int)) json    == Ok (Just 42)
    decodeString (succeed identity |> field "name" [] (maybe int)) json   == Ok Nothing
    decodeString (succeed identity |> field "height" [] (maybe int)) json == Err ...

Notice the last example!
It is saying we must have a field named `height` and the content may be a float.
There is no `height` field, so the decoder fails.

Point is, `maybe` will make exactly what it contains conditional.
For optional fields, this means you probably want it outside a use of `field` or `at`.

-}
maybe : Selector a -> Selector (Maybe a)
maybe (Selector query decoder) =
    Selector query (Json.maybe decoder)


{-| -}
render : Selector a -> Maybe String
render (Selector query _) =
    query


{-| -}
decodeString : Selector a -> String -> Result String a
decodeString (Selector _ decoder) str =
    Json.decodeString decoder str


{-| -}
decodeValue : Selector a -> Json.Value -> Result String a
decodeValue (Selector _ decoder) val =
    Json.decodeValue decoder val


{-| -}
succeed : a -> Selector a
succeed =
    Selector Nothing << Json.succeed
