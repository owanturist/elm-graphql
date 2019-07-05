module Internal exposing
    ( Argument(..)
    , argumentToString
    , renderPairsOfArguments
    , wrap
    )

import Json.Encode as Json exposing (encode)


wrap : String -> String -> String -> String
wrap prefix postfix str =
    prefix ++ str ++ postfix


type Argument
    = String String
    | Int Int
    | Float Float
    | Bool Bool
    | Null
    | Array (List Argument)
    | Object (List ( String, Argument ))


argumentToString : Argument -> String
argumentToString argument =
    case argument of
        String string ->
            encode 0 (Json.string string)

        Int int ->
            encode 0 (Json.int int)

        Float float ->
            encode 0 (Json.float float)

        Bool bool ->
            encode 0 (Json.bool bool)

        Null ->
            encode 0 Json.null

        Array listOfArguments ->
            renderListOfArguments listOfArguments

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
