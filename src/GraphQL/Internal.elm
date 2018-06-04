module GraphQL.Internal exposing (Argument(..), argumentToString, namedArgumentsToString)


type Argument
    = Argument String


argumentToString : Argument -> String
argumentToString (Argument str) =
    str


namedArgumentsToString : List ( String, Argument ) -> String
namedArgumentsToString =
    List.map (\( key, Argument input ) -> key ++ ":" ++ input)
        >> String.join ","
