module GraphQL.Argument exposing
    ( Argument, Value
    , string, int, float, bool, null
    , list, array
    , object
    , toValue
    )

{-| Define GraphQL inputs in Elm


# Primitives

@docs Argument, Value
@docs string, int, float, bool, null


# Arrays

@docs list, array


# Objects

@docs object


# Conversions

@docs toValue

-}

import Array exposing (Array)
import GraphQL.Internal as Internal
import Json.Encode as Json


{-| Represents a JavaScript value.
-}
type alias Value =
    Json.Value


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
    Internal.String


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
    Internal.Int


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
    Internal.Float


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
    Internal.Bool


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
    Internal.Null


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
    Internal.Object


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
list : (a -> Argument) -> List a -> Argument
list argument =
    Internal.List << List.map argument


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
array : (a -> Argument) -> Array a -> Argument
array argument =
    Internal.Array << Array.map argument


{-| Convert `Argument` into `Value`.

    toValue (string "hello") == Json.Encode.string "hello"

    toValue (bool True) == Json.Encode.bool True

    toValue null == Json.Encode.null

    toValue [ int 1, float 3.14 ] == Json.Encode.list [ Json.Encode.int 1, Json.Encode.float 3.14 ]

-}
toValue : Argument -> Value
toValue argument =
    case argument of
        Internal.String x ->
            Json.string x

        Internal.Int x ->
            Json.int x

        Internal.Float x ->
            Json.float x

        Internal.Bool x ->
            Json.bool x

        Internal.Null ->
            Json.null

        Internal.List listOfArguments ->
            Json.list toValue listOfArguments

        Internal.Array arrayOfArguments ->
            Json.array toValue arrayOfArguments

        Internal.Object objectConfiguration ->
            Json.object (List.map (Tuple.mapSecond toValue) objectConfiguration)
