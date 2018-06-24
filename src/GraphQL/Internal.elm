module GraphQL.Internal
    exposing
        ( Argument(..)
        , argumentToString
        , renderArguments
        , wrap
        )


wrap : String -> String -> String -> String
wrap prefix postfix str =
    prefix ++ str ++ postfix


type Argument
    = Argument String


argumentToString : Argument -> String
argumentToString (Argument str) =
    str


renderArguments : List ( String, Argument ) -> Maybe String
renderArguments arguments =
    if List.isEmpty arguments then
        Nothing
    else
        arguments
            |> List.map (\( key, Argument input ) -> key ++ ":" ++ input)
            |> String.join ","
            |> Just
