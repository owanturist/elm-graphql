module GraphQL.Selector
    exposing
        ( Selector
        , Value
        , aliased
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
        , list
        , map
        , maybe
        , null
        , nullable
        , oneOf
        , render
        , singleton
        , string
        , succeed
        , toDecoder
        , value
        )

{-| Build GraphQL with decoders for turning JSON values into Elm values.


# Primitives

@docs Selector
@docs string, bool, int, float


# Data Structures

@docs nullable, list, array, dict, keyValuePairs


# Object Primitives

@docs field, aliased, index, singleton


# Inconsistent Structure

@docs maybe, oneOf


# Run Selectors

@docs Value
@docs render, toDecoder, decodeString, decodeValue


# Fancy Decoding

@docs map, andThen, succeed, fail, value, null

-}

import Array exposing (Array)
import Dict exposing (Dict)
import GraphQL.Internal as Internal exposing (Argument)
import Json.Decode as Json exposing (Decoder)


{-| A value that knows how to select (receive and decode) a GraphQL values.
-}
type Selector a
    = Selector (Maybe String) (Decoder a)
    | Field (Maybe String) String (List ( String, Argument )) (Maybe String) (String -> Selector a)


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
nullable selector =
    case selector of
        Selector graph decoder ->
            Selector graph (Json.nullable decoder)

        Field alias name arguments prev callSelector ->
            Field alias name arguments prev (nullable << callSelector)


{-| Decode a JSON array into an Elm `List`.

    decodeString (list int) "[1,2,3]"       == Ok [1,2,3]
    decodeString (list bool) "[true,false]" == Ok [True,False]

-}
list : Selector a -> Selector (List a)
list selector =
    case selector of
        Selector graph decoder ->
            Selector graph (Json.list decoder)

        Field alias name arguments prev callSelector ->
            Field alias name arguments prev (list << callSelector)


{-| Decode a JSON array into an Elm `Array`.

    decodeString (array int) "[1,2,3]"       == Ok (Array.fromList [1,2,3])
    decodeString (array bool) "[true,false]" == Ok (Array.fromList [True,False])

-}
array : Selector a -> Selector (Array a)
array selector =
    case selector of
        Selector graph decoder ->
            Selector graph (Json.array decoder)

        Field alias name arguments prev callSelector ->
            Field alias name arguments prev (array << callSelector)


{-| Decode a JSON object into an Elm `Dict`.

    decodeString (dict int) "{ \"alice\": 42, \"bob\": 99 }"
      == Dict.fromList [("alice", 42), ("bob", 99)]

-}
dict : Selector a -> Selector (Dict String a)
dict selector =
    case selector of
        Selector graph decoder ->
            Selector graph (Json.dict decoder)

        Field alias name arguments prev callSelector ->
            Field alias name arguments prev (dict << callSelector)


{-| Decode a JSON object into an Elm `List` of pairs.

    decodeString (keyValuePairs int) "{ \"alice\": 42, \"bob\": 99 }"
      == [("alice", 42), ("bob", 99)]

-}
keyValuePairs : Selector a -> Selector (List ( String, a ))
keyValuePairs selector =
    case selector of
        Selector graph decoder ->
            Selector graph (Json.keyValuePairs decoder)

        Field alias name arguments prev callSelector ->
            Field alias name arguments prev (keyValuePairs << callSelector)


select : Maybe String -> String -> List ( String, Argument ) -> Selector a -> Selector (a -> b) -> Selector b
select alias name arguments selector next =
    (\fieldName ->
        Json.map2 (|>)
            (Json.field fieldName (toDecoder selector))
            (toDecoder next)
            |> Selector (getGraph selector)
    )
        |> Field alias name arguments (getGraph next)


{-| -}
field : String -> List ( String, Argument ) -> Selector a -> Selector (a -> b) -> Selector b
field =
    select Nothing


{-| -}
aliased : String -> String -> List ( String, Argument ) -> Selector a -> Selector (a -> b) -> Selector b
aliased =
    select << Just


{-| Version of `field` for selection single field.

    singleton "id" [] string

    -- equals to

    succeed identity
        |> field "id" [] string

-}
singleton : String -> List ( String, Argument ) -> Selector a -> Selector a
singleton name arguments selector =
    field name arguments selector (succeed identity)


{-| Decode a JSON array, requiring a particular index.

    json = """[ "alice", "bob", "chuck" ]"""

    decodeString (index 0 string) json  == Ok "alice"
    decodeString (index 1 string) json  == Ok "bob"
    decodeString (index 2 string) json  == Ok "chuck"
    decodeString (index 3 string) json  == Err ...

-}
index : Int -> Selector a -> Selector a
index i selector =
    case selector of
        Selector graph decoder ->
            Selector graph (Json.index i decoder)

        Field alias name arguments prev callSelector ->
            Field alias name arguments prev (index i << callSelector)


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
maybe selector =
    case selector of
        Selector graph decoder ->
            Selector graph (Json.maybe decoder)

        Field alias name arguments prev callSelector ->
            Field alias name arguments prev (maybe << callSelector)


{-| Try a bunch of different decoders. This can be useful if the JSON may come
in a couple different formats. For example, say you want to read an array of
numbers, but some of them are `null`.

    badInt : Decoder Int
    badInt =
        oneOf [ int, null 0 ]


    -- decodeString (list badInt) "[1,2,null,4]" == Ok [1,2,0,4]

Why would someone generate JSON like this? Questions like this are not good
for your health. The point is that you can use `oneOf` to handle situations
like this!

You could also use `oneOf` to help version your data. Try the latest format,
then a few older ones that you still support. You could use `andThen` to be
even more particular if you wanted.

-}
oneOf : List (Selector a) -> Selector a
oneOf selectors =
    let
        ( queries, decoders ) =
            selectors
                |> List.map (\selector -> ( getGraph selector, toDecoder selector ))
                |> List.unzip

        graph =
            case List.filterMap identity queries of
                [] ->
                    Nothing

                many ->
                    Just (String.join " " many)
    in
    Selector graph (Json.oneOf decoders)


{-| A JSON value.
-}
type alias Value =
    Json.Value


{-| Render GraphQL representation from a Selector.
-}
render : Selector a -> String
render =
    Maybe.withDefault "" << getGraph


getGraph : Selector a -> Maybe String
getGraph selector =
    case selector of
        Selector query _ ->
            query

        Field alias name arguments prev selector ->
            let
                fieldOrAlias =
                    case alias of
                        Nothing ->
                            name

                        Just alias ->
                            alias ++ ":" ++ name

                args =
                    Internal.renderPairsOfArguments arguments
                        |> Maybe.map (Internal.wrap "(" ")")
                        |> Maybe.withDefault ""

                nested =
                    Maybe.withDefault name alias
                        |> selector
                        |> getGraph
                        |> Maybe.map (Internal.wrap "{" "}")
                        |> Maybe.withDefault ""

                prevGraph =
                    case prev of
                        Nothing ->
                            ""

                        Just graph ->
                            graph ++ " "
            in
            Just (prevGraph ++ fieldOrAlias ++ args ++ nested)


{-| Build a Decoder from a Selector.
-}
toDecoder : Selector a -> Decoder a
toDecoder selector =
    case selector of
        Selector _ decoder ->
            decoder

        Field alias name _ _ callSelector ->
            toDecoder (callSelector (Maybe.withDefault name alias))


{-| Parse the given string into a JSON value and then run the Decoder on it.
This will fail if the string is not well-formed JSON or if the Decoder fails for some reason.

    decodeString int "4"     == Ok 4
    decodeString int "1 + 2" == Err ...

-}
decodeString : Selector a -> String -> Result String a
decodeString selector str =
    Json.decodeString (toDecoder selector) str


{-| Run a Decoder on some JSON Value.
You can send these JSON values through ports,
so that is probably the main time you would use this function.
-}
decodeValue : Selector a -> Value -> Result String a
decodeValue selector val =
    Json.decodeValue (toDecoder selector) val


{-| Transform a decoder. Maybe you just want to know the length of a string:

    stringLength : Selector Int
    stringLength =
        map String.length string

It is often helpful to use `map` with `oneOf`, like when defining `nullable`:

    nullable : Selector a -> Selector (Maybe a)
    nullable decoder =
        oneOf
            [ null Nothing
            , map Just decoder
            ]

-}
map : (a -> b) -> Selector a -> Selector b
map fn selector =
    case selector of
        Selector graph decoder ->
            Selector graph (Json.map fn decoder)

        Field alias name arguments prev callSelector ->
            Field alias name arguments prev (map fn << callSelector)


{-| Create decoders that depend on previous results.
Doesn't create depended Selector.
-}
andThen : (a -> Selector b) -> Selector a -> Selector b
andThen fn selector =
    case selector of
        Selector graph decoder ->
            Selector graph (Json.andThen (toDecoder << fn) decoder)

        Field alias name arguments prev callSelector ->
            Field alias name arguments prev (andThen fn << callSelector)


{-| Ignore the JSON and produce a certain Elm value.

    decodeString (succeed 42) "true"    == Ok 42
    decodeString (succeed 42) "[1,2,3]" == Ok 42
    decodeString (succeed 42) "hello"   == Err ... -- this is not a valid JSON string

    This is handy when used with `oneOf` or `andThen`.

-}
succeed : a -> Selector a
succeed =
    Selector Nothing << Json.succeed


{-| Ignore the JSON and make the decoder fail. This is handy when used with
`oneOf` or `andThen` where you want to give a custom error message in some
case.

See the [`andThen`](#andThen) docs for an example.

-}
fail : String -> Selector a
fail =
    Selector Nothing << Json.fail


{-| Do not do anything with a JSON value, just bring it into Elm as a `Value`.
This can be useful if you have particularly crazy data that you would like to
deal with later. Or if you are going to send it out a port and do not care
about its structure.
-}
value : Selector Value
value =
    Selector Nothing Json.value


{-| Decode a `null` value into some Elm value.

    decodeString (null False) "null" == Ok False
    decodeString (null 42) "null"    == Ok 42
    decodeString (null 42) "42"      == Err ..
    decodeString (null 42) "false"   == Err ..

So if you ever see a `null`, this will return whatever value you specified.

-}
null : a -> Selector a
null =
    Selector Nothing << Json.null
