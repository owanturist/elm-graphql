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
        , render
        , string
        , succeed
        )

{-| Build GraphQL with decoders for turning JSON values into Elm values.


# Primitives

@docs Selector
@docs string, bool, int, float


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

    selector : Selector ( String, String )
    selector =
        Selector.succeed (,)
            |> Selector.field "first" [] Selector.string
            |> Selector.field "second" [] Selector.string

-}
string : Selector String
string =
    Selector Nothing Json.string


{-| Decode a JSON string into an Elm `Bool`.

    selector : Selector ( Bool, Bool )
    selector =
        Selector.succeed (,)
            |> Selector.field "first" [] Selector.bool
            |> Selector.field "second" [] Selector.bool

-}
bool : Selector Bool
bool =
    Selector Nothing Json.bool


{-| Decode a JSON number into an Elm `Int`.

    selector : Selector ( Int, Int )
    selector =
        Selector.succeed (,)
            |> Selector.field "first" [] Selector.int
            |> Selector.field "second" [] Selector.int

-}
int : Selector Int
int =
    Selector Nothing Json.int


{-| Decode a JSON number into an Elm `Float`.

    selector : Selector ( Float, Float )
    selector =
        Selector.succeed (,)
            |> Selector.field "first" [] Selector.float
            |> Selector.field "second" [] Selector.float

-}
float : Selector Float
float =
    Selector Nothing Json.float


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
