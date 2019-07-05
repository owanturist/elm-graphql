module Argument exposing
    ( arrayOfTest
    , arrayTest
    , boolTest
    , dictOfTest
    , floatTest
    , intTest
    , issuesTest
    , listOfTest
    , listTest
    , nullTest
    , nullableTest
    , objectTest
    , setOfArray
    , stringTest
    , toValueTest
    )

import Array
import Dict
import Expect exposing (Expectation)
import Fuzz
import GraphQL.Argument as Argument
import Internal
import Json.Encode as Encode exposing (encode)
import Set
import Test exposing (Test, describe, fuzz, test)


stringTest : Test
stringTest =
    fuzz Fuzz.string "GraphQL.Argument.string" <|
        \value ->
            Argument.string value
                |> Internal.argumentToString
                |> Expect.equal (encode 0 (Encode.string value))


intTest : Test
intTest =
    fuzz Fuzz.int "GraphQL.Argument.int" <|
        \value ->
            Argument.int value
                |> Internal.argumentToString
                |> Expect.equal (String.fromInt value)


floatTest : Test
floatTest =
    fuzz Fuzz.float "GraphQL.Argument.float" <|
        \value ->
            Argument.float value
                |> Internal.argumentToString
                |> Expect.equal (String.fromFloat value)


boolTest : Test
boolTest =
    describe "GraphQL.Argument.bool"
        [ test "False" <|
            \_ ->
                Argument.bool False
                    |> Internal.argumentToString
                    |> Expect.equal "false"
        , test "True" <|
            \_ ->
                Argument.bool True
                    |> Internal.argumentToString
                    |> Expect.equal "true"
        ]


nullTest : Test
nullTest =
    test "GraphQL.Argument.null" <|
        \_ ->
            Argument.null
                |> Internal.argumentToString
                |> Expect.equal "null"


nullableTest : Test
nullableTest =
    describe "GraphQL.Argument.nullable"
        [ test "Nothing" <|
            \_ ->
                Argument.nullable Argument.int Nothing
                    |> Internal.argumentToString
                    |> Expect.equal "null"
        , fuzz Fuzz.int "Just Int" <|
            \value ->
                Argument.nullable Argument.int (Just value)
                    |> Internal.argumentToString
                    |> Expect.equal (String.fromInt value)
        ]


objectTest : Test
objectTest =
    test "GraphQL.Argument.object" <|
        \_ ->
            Argument.object
                [ ( "asString", Argument.string "str" )
                , ( "asInt", Argument.int 1 )
                , ( "asFloat", Argument.float 3.14 )
                , ( "asBool", Argument.bool True )
                , ( "asNull", Argument.null )
                ]
                |> Internal.argumentToString
                |> Expect.equal """{asString:"str",asInt:1,asFloat:3.14,asBool:true,asNull:null}"""


dictOfTest : Test
dictOfTest =
    test "GraphQL.Argument.dictOf" <|
        \_ ->
            [ ( 2, "second" )
            , ( 1, "first" )
            , ( 3, "third" )
            , ( 2, "second" )
            ]
                |> Dict.fromList
                |> Argument.dictOf String.fromInt Argument.string
                |> Internal.argumentToString
                |> Expect.equal """{1:"first",2:"second",3:"third"}"""


listTest : Test
listTest =
    test "GraphQL.Argument.list" <|
        \_ ->
            Argument.list
                [ Argument.string "str"
                , Argument.int 1
                , Argument.float 3.14
                , Argument.list
                    [ Argument.bool True
                    , Argument.null
                    ]
                ]
                |> Internal.argumentToString
                |> Expect.equal """["str",1,3.14,[true,null]]"""


listOfTest : Test
listOfTest =
    test "GraphQL.Argument.listOf" <|
        \_ ->
            Argument.listOf Argument.int [ 2, 3, 0, 1 ]
                |> Internal.argumentToString
                |> Expect.equal """[2,3,0,1]"""


arrayTest : Test
arrayTest =
    test "GraphQL.Argument.array" <|
        \_ ->
            Argument.array
                (Array.fromList
                    [ Argument.string "str"
                    , Argument.int 1
                    , Argument.float 3.14
                    , Argument.array
                        (Array.fromList
                            [ Argument.bool True
                            , Argument.null
                            ]
                        )
                    ]
                )
                |> Internal.argumentToString
                |> Expect.equal """["str",1,3.14,[true,null]]"""


arrayOfTest : Test
arrayOfTest =
    test "GraphQL.Argument.arrayOf" <|
        \_ ->
            Argument.arrayOf Argument.int (Array.fromList [ 2, 3, 0, 1 ])
                |> Internal.argumentToString
                |> Expect.equal """[2,3,0,1]"""


setOfArray : Test
setOfArray =
    test "GraphQL.Argument.setOf" <|
        \_ ->
            Argument.setOf Argument.int (Set.fromList [ 2, 3, 2, 0, 1 ])
                |> Internal.argumentToString
                |> Expect.equal """[0,1,2,3]"""


issuesTest : Test
issuesTest =
    describe "Issues"
        [ test "https://github.com/owanturist/elm-graphql/issues/1" <|
            \_ ->
                Argument.string "first\nsecond"
                    |> Internal.argumentToString
                    |> Expect.equal "\"first\\nsecond\""
        ]


toValueTest : Test
toValueTest =
    describe "GraphQL.Argument.toValue"
        [ test "to be shure that <internal> could be checked" <|
            \_ ->
                Argument.string "one"
                    |> Argument.toValue
                    |> Expect.notEqual (Encode.string "another")

        --
        , fuzz Fuzz.string "to Json.Encode.string" <|
            \value ->
                Argument.string value
                    |> Argument.toValue
                    |> Expect.equal (Encode.string value)

        --
        , fuzz Fuzz.int "to Json.Encode.int" <|
            \value ->
                Argument.int value
                    |> Argument.toValue
                    |> Expect.equal (Encode.int value)

        --
        , fuzz Fuzz.float "to Json.Encode.float" <|
            \value ->
                Argument.float value
                    |> Argument.toValue
                    |> Expect.equal (Encode.float value)

        --
        , fuzz Fuzz.bool "to Json.Encode.bool" <|
            \value ->
                Argument.bool value
                    |> Argument.toValue
                    |> Expect.equal (Encode.bool value)

        --
        , test "to Json.Encode.null" <|
            \_ ->
                Argument.null
                    |> Argument.toValue
                    |> Expect.equal Encode.null

        --
        , fuzz (Fuzz.tuple3 ( Fuzz.string, Fuzz.float, Fuzz.bool )) "to Json.Encode.list" <|
            \( string, float, bool ) ->
                Argument.list
                    [ Argument.string string
                    , Argument.float float
                    , Argument.bool bool
                    , Argument.null
                    , Argument.list
                        [ Argument.string "list"
                        , Argument.int 0
                        , Argument.float 3.14
                        , Argument.bool True
                        ]
                    , Argument.array
                        (Array.fromList
                            [ Argument.string "list"
                            , Argument.int 0
                            , Argument.float 3.14
                            , Argument.bool True
                            ]
                        )
                    , Argument.object
                        [ ( "key", Argument.string "value" )
                        ]
                    ]
                    |> Argument.toValue
                    |> Expect.equal
                        (Encode.list identity
                            [ Encode.string string
                            , Encode.float float
                            , Encode.bool bool
                            , Encode.null
                            , Encode.list identity
                                [ Encode.string "list"
                                , Encode.int 0
                                , Encode.float 3.14
                                , Encode.bool True
                                ]
                            , Encode.array identity
                                (Array.fromList
                                    [ Encode.string "list"
                                    , Encode.int 0
                                    , Encode.float 3.14
                                    , Encode.bool True
                                    ]
                                )
                            , Encode.object
                                [ ( "key", Encode.string "value" )
                                ]
                            ]
                        )

        --
        , fuzz (Fuzz.tuple3 ( Fuzz.string, Fuzz.float, Fuzz.bool )) "to Json.Encode.object" <|
            \( string, float, bool ) ->
                [ ( "string", Argument.string string )
                , ( "float", Argument.float float )
                , ( "bool", Argument.bool bool )
                , ( "null", Argument.null )
                , ( "list"
                  , Argument.list
                        [ Argument.string "list"
                        , Argument.int 0
                        , Argument.float 3.14
                        , Argument.bool True
                        ]
                  )
                , ( "array"
                  , Argument.array
                        (Array.fromList
                            [ Argument.string "list"
                            , Argument.int 0
                            , Argument.float 3.14
                            , Argument.bool True
                            ]
                        )
                  )
                , ( "object"
                  , Argument.object
                        [ ( "key", Argument.string "value" )
                        ]
                  )
                ]
                    |> Argument.object
                    |> Argument.toValue
                    |> Expect.equal
                        ([ ( "string", Encode.string string )
                         , ( "float", Encode.float float )
                         , ( "bool", Encode.bool bool )
                         , ( "null", Encode.null )
                         , ( "list"
                           , Encode.list identity
                                [ Encode.string "list"
                                , Encode.int 0
                                , Encode.float 3.14
                                , Encode.bool True
                                ]
                           )
                         , ( "array"
                           , Encode.array identity
                                (Array.fromList
                                    [ Encode.string "list"
                                    , Encode.int 0
                                    , Encode.float 3.14
                                    , Encode.bool True
                                    ]
                                )
                           )
                         , ( "object"
                           , Encode.object
                                [ ( "key", Encode.string "value" )
                                ]
                           )
                         ]
                            |> Encode.object
                        )
        ]
