module GraphQL.Internal
    exposing
        ( Argument(..)
        , argumentToString
        , renderPairsOfArguments
        , wrap
        )

import Array exposing (Array)


wrap : String -> String -> String -> String
wrap prefix postfix str =
    prefix ++ str ++ postfix


type Argument
    = String String
    | Int Int
    | Float Float
    | Bool Bool
    | Null
    | List (List Argument)
    | Array (Array Argument)
    | Object (List ( String, Argument ))


argumentToString : Argument -> String
argumentToString argument =
    case argument of
        String string ->
            wrap "\"" "\"" string

        Int int ->
            toString int

        Float float ->
            toString float

        Bool bool ->
            String.toLower (toString bool)

        Null ->
            "null"

        List listOfArguments ->
            renderListOfArguments listOfArguments

        Array arrayOfArguments ->
            renderListOfArguments (Array.toList arrayOfArguments)

        Object objectConfiguration ->
            renderPairsOfArguments objectConfiguration
                |> Maybe.withDefault ""
                |> wrap "{" "}"


renderListOfArguments : List Argument -> String
renderListOfArguments =
    List.map argumentToString
        >> String.join ","
        >> wrap "[" "]"


renderPairsOfArguments : List ( String, Argument ) -> Maybe String
renderPairsOfArguments arguments =
    if List.isEmpty arguments then
        Nothing
    else
        arguments
            |> List.map (\( key, argument ) -> key ++ ":" ++ argumentToString argument)
            |> String.join ","
            |> Just
