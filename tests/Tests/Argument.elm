module Tests.Argument exposing (tests)

import Array
import Expect exposing (Expectation)
import Fuzz
import GraphQL.Argument as Argument
import GraphQL.Internal as Internal
import Json.Encode as Encode exposing (encode)
import Test exposing (Test, describe, fuzz, test)


tests : Test
tests =
    describe "GraphQL.Argument"
        [ fuzz Fuzz.string "GraphQL.Argument.string" <|
            \value ->
                Argument.string value
                    |> Internal.argumentToString
                    |> Expect.equal (encode 0 (Encode.string value))
        , fuzz Fuzz.int "GraphQL.Argument.int" <|
            \value ->
                Argument.int value
                    |> Internal.argumentToString
                    |> Expect.equal (String.fromInt value)
        , fuzz Fuzz.float "GraphQL.Argument.float" <|
            \value ->
                Argument.float value
                    |> Internal.argumentToString
                    |> Expect.equal (String.fromFloat value)
        , test "GraphQL.Argument.bool False" <|
            \_ ->
                Argument.bool False
                    |> Internal.argumentToString
                    |> Expect.equal "false"
        , test "GraphQL.Argument.bool True" <|
            \_ ->
                Argument.bool True
                    |> Internal.argumentToString
                    |> Expect.equal "true"
        , test "GraphQL.Argument.null" <|
            \_ ->
                Argument.null
                    |> Internal.argumentToString
                    |> Expect.equal "null"
        , test "GraphQL.Argument.object" <|
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
        , test "GraphQL.Argument.list" <|
            \_ ->
                Argument.list identity
                    [ Argument.string "str"
                    , Argument.int 1
                    , Argument.float 3.14
                    , Argument.list identity
                        [ Argument.bool True
                        , Argument.null
                        ]
                    ]
                    |> Internal.argumentToString
                    |> Expect.equal """["str",1,3.14,[true,null]]"""
        , test "GraphQL.Argument.array" <|
            \_ ->
                Argument.array identity
                    (Array.fromList
                        [ Argument.string "str"
                        , Argument.int 1
                        , Argument.float 3.14
                        , Argument.array identity
                            (Array.fromList
                                [ Argument.bool True
                                , Argument.null
                                ]
                            )
                        ]
                    )
                    |> Internal.argumentToString
                    |> Expect.equal """["str",1,3.14,[true,null]]"""
        , test "ISSUE https://github.com/owanturist/elm-graphql/issues/1" <|
            \_ ->
                Argument.string "first\nsecond"
                    |> Internal.argumentToString
                    |> Expect.equal "\"first\\nsecond\""
        , describe "GraphQL.Argument.toValue" toValueTests
        ]


toValueTests : List Test
toValueTests =
    [ test "fail test" <|
        \_ ->
            Argument.string "one"
                |> Argument.toValue
                |> Expect.notEqual (Encode.string "another")
    , fuzz Fuzz.string "GraphQL.Argument.string" <|
        \value ->
            Argument.string value
                |> Argument.toValue
                |> Expect.equal (Encode.string value)
    , fuzz Fuzz.int "GraphQL.Argument.int" <|
        \value ->
            Argument.int value
                |> Argument.toValue
                |> Expect.equal (Encode.int value)
    , fuzz Fuzz.float "GraphQL.Argument.float" <|
        \value ->
            Argument.float value
                |> Argument.toValue
                |> Expect.equal (Encode.float value)
    , fuzz Fuzz.bool "GraphQL.Argument.bool" <|
        \value ->
            Argument.bool value
                |> Argument.toValue
                |> Expect.equal (Encode.bool value)
    , test "GraphQL.Argument.null" <|
        \_ ->
            Argument.null
                |> Argument.toValue
                |> Expect.equal Encode.null
    , fuzz (Fuzz.tuple3 ( Fuzz.string, Fuzz.float, Fuzz.bool )) "GraphQL.Argument.list" <|
        \( string, float, bool ) ->
            Argument.list identity
                [ Argument.string string
                , Argument.float float
                , Argument.bool bool
                , Argument.null
                , Argument.list identity
                    [ Argument.string "list"
                    , Argument.int 0
                    , Argument.float 3.14
                    , Argument.bool True
                    ]
                , Argument.array identity
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
    , fuzz (Fuzz.tuple3 ( Fuzz.string, Fuzz.int, Fuzz.bool )) "GraphQL.Argument.array" <|
        \( string, int, bool ) ->
            [ Argument.string string
            , Argument.int int
            , Argument.bool bool
            , Argument.null
            , Argument.list identity
                [ Argument.string "list"
                , Argument.int 0
                , Argument.float 3.14
                , Argument.bool True
                ]
            , Argument.array identity
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
                |> Array.fromList
                |> Argument.array identity
                |> Argument.toValue
                |> Expect.equal
                    ([ Encode.string string
                     , Encode.int int
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
                        |> Array.fromList
                        |> Encode.array identity
                    )
    , fuzz (Fuzz.tuple3 ( Fuzz.string, Fuzz.float, Fuzz.bool )) "GraphQL.Argument.object" <|
        \( string, float, bool ) ->
            [ ( "string", Argument.string string )
            , ( "float", Argument.float float )
            , ( "bool", Argument.bool bool )
            , ( "null", Argument.null )
            , ( "list"
              , Argument.list identity
                    [ Argument.string "list"
                    , Argument.int 0
                    , Argument.float 3.14
                    , Argument.bool True
                    ]
              )
            , ( "array"
              , Argument.array identity
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
