module GraphQL.Selector
    exposing
        ( ($>)
        , Selector
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
        , on
        , oneOf
        , render
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

@docs ($>), field, aliased, index, on


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


{-| A JSON value.
-}
type alias Value =
    Json.Value


{-| A value that knows how to select (receive and decode) a GraphQL values.
-}
type Selector a
    = Selector (Maybe String) (Decoder a)


primitive : Decoder a -> Selector a
primitive =
    Selector Nothing


container : (Decoder a -> Decoder b) -> Selector a -> Selector b
container wrapper (Selector query decoder) =
    Selector query (wrapper decoder)


infixl 0 $>


{-| -}
($>) : Selector (a -> b) -> Selector a -> Selector b
($>) next selector =
    map2 (|>) selector next


filterJoinQueries : List (Maybe String) -> Maybe String
filterJoinQueries queries =
    case List.filterMap identity queries of
        [] ->
            Nothing

        [ single ] ->
            Just single

        many ->
            Just (joinQueries many)


joinQueries : List String -> String
joinQueries =
    String.join " "


{-| Transform a selector. Maybe you just want to know the length of a string:

    stringLength : Selector Int
    stringLength =
        map String.length string

It is often helpful to use `map` with `oneOf`, like when defining `nullable`:

    nullable : Selector a -> Selector (Maybe a)
    nullable selector =
        oneOf
            [ null Nothing
            , map Just selector
            ]

-}
map : (a -> b) -> Selector a -> Selector b
map fn (Selector query decoder) =
    Selector query (Json.map fn decoder)


{-| Try two selectors and then combine the result
We can use this to select objects with many fields:

    type alias Point =
        { x : Float, y : Float }

    point : Selector Point
    point =
        map2 Point
            (field "x" [] float)
            (field "y" [] float)


    -- decodeString point """{ "x": 3, "y": 4 }""" == Ok { x = 3, y = 4 }

It tries each individual decoder and puts the result together with the `Point`
constructor.

-}
map2 : (a -> b -> c) -> Selector a -> Selector b -> Selector c
map2 fn (Selector qA dA) (Selector qB dB) =
    Selector
        (filterJoinQueries [ qB, qA ])
        (Json.map2 fn dA dB)


{-| Try three selectors and then combine the result.
We can use this to select objects with many fields:

    type alias Person =
        { name : String, age : Int, height : Float }

    person : Selector Person
    person =
        map3 Person
            (field "name" [] string)
            (field age" [] int)
            (field "height" [] float)


    -- json = """{ "name": "tom", "info": { "age": 42, "height": 1.8 } }"""
    -- decodeString person json == Ok { name = "tom", age = 42, height = 1.8 }

Like `map2` it tries each decoder in order and then give the results to the
`Person` constructor. That can be any function though!

-}
map3 : (a -> b -> c -> d) -> Selector a -> Selector b -> Selector c -> Selector d
map3 fn (Selector qA dA) (Selector qB dB) (Selector qC dC) =
    Selector
        (filterJoinQueries [ qC, qB, qA ])
        (Json.map3 fn dA dB dC)


{-| -}
map4 : (a -> b -> c -> d -> e) -> Selector a -> Selector b -> Selector c -> Selector d -> Selector e
map4 fn (Selector qA dA) (Selector qB dB) (Selector qC dC) (Selector qD dD) =
    Selector
        (filterJoinQueries [ qD, qC, qB, qA ])
        (Json.map4 fn dA dB dC dD)


{-| -}
map5 : (a -> b -> c -> d -> e -> f) -> Selector a -> Selector b -> Selector c -> Selector d -> Selector e -> Selector f
map5 fn (Selector qA dA) (Selector qB dB) (Selector qC dC) (Selector qD dD) (Selector qE dE) =
    Selector
        (filterJoinQueries [ qE, qD, qC, qB, qA ])
        (Json.map5 fn dA dB dC dD dE)


{-| -}
map6 : (a -> b -> c -> d -> e -> f -> g) -> Selector a -> Selector b -> Selector c -> Selector d -> Selector e -> Selector f -> Selector g
map6 fn (Selector qA dA) (Selector qB dB) (Selector qC dC) (Selector qD dD) (Selector qE dE) (Selector qF dF) =
    Selector
        (filterJoinQueries [ qF, qE, qD, qC, qB, qA ])
        (Json.map6 fn dA dB dC dD dE dF)


{-| -}
map7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Selector a -> Selector b -> Selector c -> Selector d -> Selector e -> Selector f -> Selector g -> Selector h
map7 fn (Selector qA dA) (Selector qB dB) (Selector qC dC) (Selector qD dD) (Selector qE dE) (Selector qF dF) (Selector qG dG) =
    Selector
        (filterJoinQueries [ qG, qF, qE, qD, qC, qB, qA ])
        (Json.map7 fn dA dB dC dD dE dF dG)


{-| -}
map8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Selector a -> Selector b -> Selector c -> Selector d -> Selector e -> Selector f -> Selector g -> Selector h -> Selector i
map8 fn (Selector qA dA) (Selector qB dB) (Selector qC dC) (Selector qD dD) (Selector qE dE) (Selector qF dF) (Selector qG dG) (Selector qI dI) =
    Selector
        (filterJoinQueries [ qI, qG, qF, qE, qD, qC, qB, qA ])
        (Json.map8 fn dA dB dC dD dE dF dG dI)


{-| Create a selector that depend on previous results.
Doesn't create depended Selector.
-}
andThen : (a -> Selector b) -> Selector a -> Selector b
andThen fn (Selector query decoder) =
    Selector query (Json.andThen (toDecoder << fn) decoder)


{-| Decode a JSON string into an Elm `String`.

    decodeString string "true"              == Err ...
    decodeString string "42"                == Err ...
    decodeString string "3.14"              == Err ...
    decodeString string "\"hello\""         == Ok "hello"
    decodeString string "{ \"hello\": 42 }" == Err ...

-}
string : Selector String
string =
    primitive Json.string


{-| Decode a JSON string into an Elm `Bool`.

    decodeString bool "true"              == Ok True
    decodeString bool "42"                == Err ...
    decodeString bool "3.14"              == Err ...
    decodeString bool "\"hello\""         == Err ...
    decodeString bool "{ \"hello\": 42 }" == Err ...

-}
bool : Selector Bool
bool =
    primitive Json.bool


{-| Decode a JSON number into an Elm `Int`.

    decodeString int "true"              == Err ...
    decodeString int "42"                == Ok 42
    decodeString int "3.14"              == Err ...
    decodeString int "\"hello\""         == Err ...
    decodeString int "{ \"hello\": 42 }" == Err ...

-}
int : Selector Int
int =
    primitive Json.int


{-| Decode a JSON number into an Elm `Float`.

    decodeString float "true"              == Err ..
    decodeString float "42"                == Ok 42
    decodeString float "3.14"              == Ok 3.14
    decodeString float "\"hello\""         == Err ...
    decodeString float "{ \"hello\": 42 }" == Err ...

-}
float : Selector Float
float =
    primitive Json.float


{-| Ignore the JSON and produce a certain Elm value.

    decodeString (succeed 42) "true"    == Ok 42
    decodeString (succeed 42) "[1,2,3]" == Ok 42
    decodeString (succeed 42) "hello"   == Err ... -- this is not a valid JSON string

    This is handy when used with `oneOf` or `andThen`.

-}
succeed : a -> Selector a
succeed =
    primitive << Json.succeed


{-| Ignore the JSON and make the selector fail. This is handy when used with
`oneOf` or `andThen` where you want to give a custom error message in some
case.

See the [`andThen`](#andThen) docs for an example.

-}
fail : String -> Selector a
fail =
    primitive << Json.fail


{-| Do not do anything with a JSON value, just bring it into Elm as a `Value`.
This can be useful if you have particularly crazy data that you would like to
deal with later. Or if you are going to send it out a port and do not care
about its structure.
-}
value : Selector Value
value =
    primitive Json.value


{-| Decode a `null` value into some Elm value.

    decodeString (null False) "null" == Ok False
    decodeString (null 42) "null"    == Ok 42
    decodeString (null 42) "42"      == Err ..
    decodeString (null 42) "false"   == Err ..

So if you ever see a `null`, this will return whatever value you specified.

-}
null : a -> Selector a
null =
    primitive << Json.null


{-| Decode a nullable JSON value into an Elm value.

    decodeString (nullable int) "13"    == Ok (Just 13)
    decodeString (nullable int) "42"    == Ok (Just 42)
    decodeString (nullable int) "null"  == Ok Nothing
    decodeString (nullable int) "true"  == Err ...

-}
nullable : Selector a -> Selector (Maybe a)
nullable =
    container Json.nullable


{-| Decode a JSON array into an Elm `List`.

    decodeString (list int) "[1,2,3]"       == Ok [1,2,3]
    decodeString (list bool) "[true,false]" == Ok [True,False]

-}
list : Selector a -> Selector (List a)
list =
    container Json.list


{-| Decode a JSON array into an Elm `Array`.

    decodeString (array int) "[1,2,3]"       == Ok (Array.fromList [1,2,3])
    decodeString (array bool) "[true,false]" == Ok (Array.fromList [True,False])

-}
array : Selector a -> Selector (Array a)
array =
    container Json.array


{-| Decode a JSON object into an Elm `Dict`.

    decodeString (dict int) "{ \"alice\": 42, \"bob\": 99 }"
      == Dict.fromList [("alice", 42), ("bob", 99)]

-}
dict : Selector a -> Selector (Dict String a)
dict =
    container Json.dict


{-| Decode a JSON object into an Elm `List` of pairs.

    decodeString (keyValuePairs int) "{ \"alice\": 42, \"bob\": 99 }"
      == [("alice", 42), ("bob", 99)]

-}
keyValuePairs : Selector a -> Selector (List ( String, a ))
keyValuePairs =
    container Json.keyValuePairs


{-| Decode a JSON array, requiring a particular index.

    json = """[ "alice", "bob", "chuck" ]"""

    decodeString (index 0 string) json  == Ok "alice"
    decodeString (index 1 string) json  == Ok "bob"
    decodeString (index 2 string) json  == Ok "chuck"
    decodeString (index 3 string) json  == Err ...

-}
index : Int -> Selector a -> Selector a
index i =
    container (Json.index i)


{-| Helpful for dealing with optional fields. Here are a few slightly different examples:

    json = """{ "name": "tom", "age": 42 }"""

    decodeString (field "age" [] (maybe int)) json    == Ok (Just 42)
    decodeString (field "name" [] (maybe int)) json   == Ok Nothing
    decodeString (field "height" [] (maybe int)) json == Err ...

Notice the last example!
It is saying we must have a field named `height` and the content may be a float.
There is no `height` field, so the decoder fails.

Point is, `maybe` will make exactly what it contains conditional.
For optional fields, this means you probably want it outside a use of `field` or `at`.

-}
maybe : Selector a -> Selector (Maybe a)
maybe =
    container Json.maybe


select : Maybe String -> String -> List ( String, Argument ) -> Selector a -> Selector a
select alias name arguments (Selector childQuery decoder) =
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

        child =
            childQuery
                |> Maybe.map (Internal.wrap "{" "}")
                |> Maybe.withDefault ""
    in
    Selector
        (Just (fieldOrAlias ++ args ++ child))
        (Json.field (Maybe.withDefault name alias) decoder)


{-| -}
field : String -> List ( String, Argument ) -> Selector a -> Selector a
field =
    select Nothing


{-| -}
aliased : String -> String -> List ( String, Argument ) -> Selector a -> Selector a
aliased =
    select << Just


{-| -}
on : List ( String, Selector a ) -> Selector a
on selectors =
    let
        ( queries, decoders ) =
            selectors
                |> List.map
                    (\( entity, Selector query decoder ) ->
                        ( "...on " ++ entity ++ Internal.wrap "{" "}" (Maybe.withDefault "" query)
                        , decoder
                        )
                    )
                |> List.unzip
    in
    Selector (Just (joinQueries queries)) (Json.oneOf decoders)


{-| Try a bunch of different selectors. This can be useful if the JSON may come
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
                |> List.map (\(Selector query decoder) -> ( query, decoder ))
                |> List.unzip
    in
    Selector (filterJoinQueries queries) (Json.oneOf decoders)


{-| Render GraphQL representation of Selector.
-}
render : Selector a -> String
render (Selector query _) =
    Maybe.withDefault "" query


{-| Build a Decoder of Selector.
-}
toDecoder : Selector a -> Decoder a
toDecoder (Selector _ decoder) =
    decoder


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
