module GraphQL.Selector
    exposing
        ( Selector
        , aliased
        , bool
        , decodeString
        , decodeValue
        , field
        , float
        , int
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

@docs nullable


# Object Primitives

@docs field, aliased


# Run Selectors

@docs render, decodeString, decodeValue


# Fancy Decoding

@docs succeed

-}

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
