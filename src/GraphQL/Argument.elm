module GraphQL.Argument exposing
    ( Argument, Value
    , string, int, float, bool, null
    , list, listOf, array, arrayOf
    , object, dictOf
    , toValue
    )

{-| Define GraphQL inputs in Elm


# Primitives

@docs Argument, Value
@docs string, int, float, bool, null, nullable


# Arrays

@docs list, listOf, array, arrayOf


# Objects

@docs object, dictOf


# Conversions

@docs toValue

-}

import Array exposing (Array)
import Dict exposing (Dict)
import GraphQL.Internal as Internal
import Json.Encode as Json
import Set exposing (Set)


{-| Represents a JavaScript value.
-}
type alias Value =
    Json.Value


{-| Represents a GraphQL argument values.
-}
type alias Argument =
    Internal.Argument


{-| Pass string argument into a graph.

    GraphQL.Selector.field "fieldName"
        [ ( "asString", GraphQL.Argument.string "foo" )
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

    GraphQL.Selector.field "fieldName"
        [ ( "asInt", GraphQL.Argument.int 1 )
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

    GraphQL.Selector.field "fieldName"
        [ ( "asFloat", GraphQL.Argument.float 3.14 )
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

    GraphQL.Selector.field "fieldName"
        [ ( "asBool", GraphQL.Argument.bool True )
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

    GraphQL.Selector.field "fieldName"
        [ ( "asNull", GraphQL.Argument.null )
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


{-| Pass nullable argument into a graph.

    GraphQL.Selector.field "fieldName"
        [ ( "asString", GraphQL.Argument.nullable GraphQL.Argument.string (Just "foo") )
        , ( "asNull", GraphQL.Argument.nullable GraphQL.Argument.string Nothing )
        ]
        GraphQL.Selector.int

Equals to:

    """
    fieldName(
        asString: "foo",
        asNull: null
    )
    """

-}
nullable : (a -> Argument) -> Maybe a -> Argument
nullable tagger argument =
    Maybe.withDefault null (Maybe.map tagger argument)


{-| Pass object of arguments into a graph.

    GraphQL.Selector.field "fieldName"
        [ ( "asObject"
          , GraphQL.Argument.object
                [ ( "asString", GraphQL.Argument.string "foo" )
                , ( "asInt", GraphQL.Argument.int 1 )
                , ( "asFloat", GraphQL.Argument.float 3.14 )
                , ( "asBool", GraphQL.Argument.bool True )
                , ( "asNull", GraphQL.Argument.null )
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


{-| Pass dict of arguments into a graph.

    GraphQL.Selector.field "fieldName"
        [ ( "asObject"
          , GraphQL.Argument.dictOf
                String.fromInt
                GraphQL.Argument.string
                (Dict.fromList
                    [ ( 2, "second" )
                    , ( 1, "first" )
                    , ( 3, "third" )
                    ]
                )
          )
        ]
        GraphQL.Selector.int

Equals to:

    """
    fieldName(asObject: {
        1: "first",
        2: "second",
        3: "third"
    })
    """

-}
dictOf : (k -> String) -> (v -> Argument) -> Dict k v -> Argument
dictOf keyTagger valueTagger arguments =
    Dict.foldr (\key value acc -> ( keyTagger key, valueTagger value ) :: acc) [] arguments
        |> Internal.Object


{-| Pass list of arguments into a graph.

    GraphQL.Selector.field "fieldName"
        [ ( "asInConsistentArray"
          , GraphQL.Argument.list
                [ GraphQL.Argument.string "foo"
                , GraphQL.Argument.int 0
                , GraphQL.Argument.list [ GraphQL.Argument.bool False ]
                ]
          )
        , ( "asConsistentArray"
          , GraphQL.Argument.list
                [ GraphQL.Argument.int 0
                , GraphQL.Argument.int 1
                , GraphQL.Argument.int 2
                ]
          )
        ]
        GraphQL.Selector.int

Equals to:

    """
    fieldName(
        asInConsistentArray: ["foo", 0, [false]],
        asConsistentArray: [0, 1, 2]
    )
    """

-}
list : List Argument -> Argument
list =
    Internal.List


{-| Pass list of specific arguments into a graph.

    GraphQL.Selector.field "fieldName"
        [ ( "asArray"
          , GraphQL.Argument.listOf GraphQL.Argument.int [ 0, 1, 2 ]
          )
        ]
        GraphQL.Selector.int

Equals to:

    """
    fieldName(
        asArray: [0, 1, 2]
    )
    """

-}
listOf : (a -> Argument) -> List a -> Argument
listOf tagger arguments =
    list (List.map tagger arguments)


{-| Pass array of arguments into a graph.

    GraphQL.Selector.field "fieldName"
        [ ( "asInConsistentArray"
          , GraphQL.Argument.array
                (Array.fromList
                    [ GraphQL.Argument.string "foo"
                    , GraphQL.Argument.int 0
                    , GraphQL.Argument.array (Array.fromList [ GraphQL.Argument.bool False ])
                    ]
                )
          )
        , ( "asConsistentArray"
          , GraphQL.Argument.array
                (Array.fromList
                    [ GraphQL.Argument.int 0
                    , GraphQL.Argument.int 1
                    , GraphQL.Argument.int 2
                    ]
                )
          )
        ]
        GraphQL.Selector.int

Equals to:

    """
    fieldName(
        asInConsistentArray: ["foo", 0, [false]],
        asConsistentArray: [0, 1, 2]
    )
    """

-}
array : Array Argument -> Argument
array =
    Internal.Array


{-| Pass array of specific arguments into a graph.

    GraphQL.Selector.field "fieldName"
        [ ( "asArray"
          , GraphQL.Argument.arrayOf GraphQL.Argument.int (Array.fromList [ 0, 1, 2 ])
          )
        ]
        GraphQL.Selector.int

Equals to:

    """
    fieldName(
        asArray: [0, 1, 2]
    )
    """

-}
arrayOf : (a -> Argument) -> Array a -> Argument
arrayOf tagger arguments =
    array (Array.map tagger arguments)


{-| Pass set of specific arguments into a graph.

    GraphQL.Selector.field "fieldName"
        [ ( "asArray"
          , GraphQL.Argument.setOf GraphQL.Argument.int (Set.fromList [ 0, 2, 1, 0 ])
          )
        ]
        GraphQL.Selector.int

Equals to:

    """
    fieldName(
        asArray: [0, 1, 2]
    )
    """

-}
setOf : (a -> Argument) -> Set a -> Argument
setOf tagger arguments =
    list (Set.foldr ((::) << tagger) [] arguments)


{-| Convert `Argument` into `Value`.

    GraphQL.Argument.toValue (GraphQL.Argument.string "hello") == Json.Encode.string "hello"

    GraphQL.Argument.toValue (GraphQL.Argument.bool True) == Json.Encode.bool True

    GraphQL.Argument.toValue GraphQL.Argument.null == Json.Encode.null

    GraphQL.Argument.toValue (GraphQL.Argument.listOf GraphQL.Argument.int [ 0, 1, 2 ])
        == Json.Encode.listOf Json.Encode.int [ 0, 1, 2 ]

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
