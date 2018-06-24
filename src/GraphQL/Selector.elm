module GraphQL.Selector exposing (..)

import GraphQL.Internal as Internal exposing (Argument)
import Json.Decode as Json exposing (Decoder)


type Selector a
    = Selector (Maybe String) (Decoder a)


string : Selector String
string =
    Selector Nothing Json.string


bool : Selector Bool
bool =
    Selector Nothing Json.bool


int : Selector Int
int =
    Selector Nothing Json.int


float : Selector Float
float =
    Selector Nothing Json.float


field : String -> List ( String, Argument ) -> Selector a -> Selector (a -> b) -> Selector b
field name arguments (Selector query1 decoder) (Selector query2 next) =
    let
        base =
            Internal.renderArguments arguments
                |> Maybe.map (Internal.wrap "(" ")")
                |> Maybe.withDefault ""
                |> (++) name

        query =
            case ( query1, query2 ) of
                ( Nothing, Nothing ) ->
                    base

                ( Nothing, Just query2 ) ->
                    base ++ " " ++ query2

                ( Just query1, Nothing ) ->
                    base ++ Internal.wrap "{" "}" query1

                ( Just query1, Just query2 ) ->
                    base ++ Internal.wrap "{" "}" query1 ++ query2
    in
    Selector (Just query) (Json.map2 (|>) decoder next)


succeed : a -> Selector a
succeed =
    Selector Nothing << Json.succeed


select : Selector a -> ( Maybe String, Decoder a )
select (Selector query decoder) =
    ( query, decoder )
