module GraphQL.Argument exposing (Argument, array, bool, float, int, list, null, object, string)

{-| Define GraphQL inputs in Elm


# Primitives

@docs Argument
@docs string, int, float, bool, null


# Arrays

@docs list, array


# Objects

@docs object

-}

import Array exposing (Array)
import GraphQL.Internal as Internal


wrap : String -> String -> String -> String
wrap prefix postfix str =
    prefix ++ str ++ postfix


{-| Represents a GraphQL input values.
-}
type alias Argument =
    Internal.Argument


{-| Pass string argument into a graph.

    GraphQL.Selector.succeed Constructor
        |> GraphQL.Selector.field "fieldName"
            [ ( "asString", string "foo" )
            ]
            GraphQL.Selector.int

Equals to:

    """
    fieldName(asString: "foo")
    """

-}
string : String -> Argument
string =
    Internal.Argument << wrap "\"" "\""


{-| Pass int argument into a graph.

    GraphQL.Selector.succeed Constructor
        |> GraphQL.Selector.field "fieldName"
            [ ( "asInt", int 1 )
            ]
            GraphQL.Selector.int

Equals to:

    """
    fieldName(asInt: 1)
    """

-}
int : Int -> Argument
int =
    Internal.Argument << toString


{-| Pass float argument into a graph.

    GraphQL.Selector.succeed Constructor
        |> GraphQL.Selector.field "fieldName"
            [ ( "asFloat", float 3.14 )
            ]
            GraphQL.Selector.int

Equals to:

    """
    fieldName(asFloat: 3.14)
    """

-}
float : Float -> Argument
float =
    Internal.Argument << toString


{-| Pass bool argument into a graph.

    GraphQL.Selector.succeed Constructor
        |> GraphQL.Selector.field "fieldName"
            [ ( "asBool", bool True )
            ]
            GraphQL.Selector.int

Equals to:

    """
    fieldName(asBool: true)
    """

-}
bool : Bool -> Argument
bool =
    toString
        >> String.toLower
        >> Internal.Argument


{-| Pass null argument into a graph.

    GraphQL.Selector.succeed Constructor
        |> GraphQL.Selector.field "fieldName"
            [ ( "asNull", null )
            ]
            GraphQL.Selector.int

Equals to:

    """
    fieldName(asNull: null)
    """

-}
null : Argument
null =
    Internal.Argument "null"


{-| Pass object of arguments into a graph.

    GraphQL.Selector.succeed Constructor
        |> GraphQL.Selector.field "fieldName"
            [ ( "asObject"
              , object
                    [ ( "asString", string "foo" )
                    , ( "asInt", int 1 )
                    , ( "asFloat", float 3.14 )
                    , ( "asBool", bool True )
                    , ( "asNull", null )
                    ]
              )
            ]
            GraphQL.Selector.int

Equals to:

    """
    fieldName(asObject: {
        asString: "foo",
        asInt: 1,
        asFloat: 3.14,
        asBool: true,
        asNull: null
    })
    """

-}
object : List ( String, Argument ) -> Argument
object =
    Internal.namedArgumentsToString
        >> wrap "{" "}"
        >> Internal.Argument


{-| Pass list of arguments into a graph.

    GraphQL.Selector.succeed Constructor
        |> GraphQL.Selector.field "fieldName"
            [ ( "asInConsistentList"
              , list
                    [ string "foo"
                    , int 0
                    , list [ bool False ]
                    ]
              )
            , ( "asConsistentList"
              , list
                    [ int 0
                    , int 1
                    , int 2
                    ]
              )
            ]
            GraphQL.Selector.int

Equals to:

    """
    fieldName(
        asInConsistentList: ["foo", 0, false],
        asConsistentList: [0, 1, 2]
    )
    """

-}
list : List Argument -> Argument
list =
    List.map Internal.argumentToString
        >> String.join ","
        >> wrap "[" "]"
        >> Internal.Argument


{-| Pass array of arguments into a graph.

    GraphQL.Selector.succeed Constructor
        |> GraphQL.Selector.field "fieldName"
            [ ( "asInConsistentArray"
              , array
                    (Array.fromList
                        [ string "foo"
                        , int 0
                        , array [ bool False ]
                        ]
                    )
              )
            , ( "asConsistentArray"
              , array
                    (Array.fromList
                        [ int 0
                        , int 1
                        , int 2
                        ]
                    )
              )
            ]
            GraphQL.Selector.int

Equals to:

    """
    fieldName(
        asInConsistentList: ["foo", 0, false],
        asConsistentList: [0, 1, 2]
    )
    """

-}
array : Array Argument -> Argument
array =
    list << Array.toList
