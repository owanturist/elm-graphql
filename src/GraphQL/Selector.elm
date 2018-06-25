module GraphQL.Selector
    exposing
        ( Selector
        , aliased
        , bool
        , field
        , float
        , int
        , select
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

@docs select


# Fancy Decoding

@docs succeed

-}

import GraphQL.Internal as Internal exposing (Argument)
import Json.Decode as Json exposing (Decoder)


{-| A value that knows how to select (receive and decode) a GraphQL values.
-}
type Selector a
    = Selector (Maybe String) (Decoder a)


{-| -}
string : Selector String
string =
    Selector Nothing Json.string


{-| -}
bool : Selector Bool
bool =
    Selector Nothing Json.bool


{-| -}
int : Selector Int
int =
    Selector Nothing Json.int


{-| -}
float : Selector Float
float =
    Selector Nothing Json.float


selector : Maybe String -> String -> List ( String, Argument ) -> Selector a -> Selector (a -> b) -> Selector b
selector alias name arguments (Selector query1 decoder) (Selector query2 next) =
    let
        field =
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
                    field ++ args

                ( Nothing, Just query2 ) ->
                    query2 ++ " " ++ field ++ args

                ( Just query1, Nothing ) ->
                    field ++ args ++ Internal.wrap "{" "}" query1

                ( Just query1, Just query2 ) ->
                    query2 ++ " " ++ field ++ args ++ Internal.wrap "{" "}" query1
    in
    Json.map2 (|>) decoder next
        |> Json.field (Maybe.withDefault name alias)
        |> Selector (Just query)


{-| -}
field : String -> List ( String, Argument ) -> Selector a -> Selector (a -> b) -> Selector b
field =
    selector Nothing


{-| -}
aliased : String -> String -> List ( String, Argument ) -> Selector a -> Selector (a -> b) -> Selector b
aliased =
    selector << Just


{-| -}
select : Selector a -> ( Maybe String, Decoder a )
select (Selector query decoder) =
    ( query, decoder )


{-| -}
succeed : a -> Selector a
succeed =
    Selector Nothing << Json.succeed
