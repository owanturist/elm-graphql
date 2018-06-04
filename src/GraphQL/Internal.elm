module GraphQL.Internal
    exposing
        ( Argument(..)
        , Selector(..)
        , argumentToString
        , namedArgumentsToString
        )

import Json.Decode as Decode exposing (Decoder)


type Argument
    = Argument String


argumentToString : Argument -> String
argumentToString (Argument str) =
    str


namedArgumentsToString : List ( String, Argument ) -> String
namedArgumentsToString =
    List.map (\( key, Argument input ) -> key ++ ":" ++ input)
        >> String.join ","


type Selector a
    = Selector
        (Maybe
            { name : String
            , alias : Maybe String
            , arguments : List ( String, Argument )
            }
        )
        (Decoder a)
