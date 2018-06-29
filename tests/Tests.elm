module Tests exposing (..)

import Array
import Dict
import Expect exposing (Expectation)
import Fuzz
import GraphQL.Argument as Argument
import GraphQL.Internal as Internal
import GraphQL.Selector as Selector exposing (Selector)
import Json.Encode as Encode
import Test exposing (Test, describe, fuzz, test)


argumentSheet : Test
argumentSheet =
    describe "Test GraphQL.Argument builder functions"
        [ fuzz Fuzz.string "GraphQL.Argument.string" <|
            \value ->
                Argument.string value
                    |> Internal.argumentToString
                    |> Expect.equal ("\"" ++ value ++ "\"")
        , fuzz Fuzz.int "GraphQL.Argument.int" <|
            \value ->
                Argument.int value
                    |> Internal.argumentToString
                    |> Expect.equal (toString value)
        , fuzz Fuzz.float "GraphQL.Argument.float" <|
            \value ->
                Argument.float value
                    |> Internal.argumentToString
                    |> Expect.equal (toString value)
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
        , test "GraphQL.Argument.array" <|
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
        ]


argumentToValueSheet : Test
argumentToValueSheet =
    describe "Test GraphQL.Argument.toValue function"
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
        , fuzz (Fuzz.tuple4 ( Fuzz.string, Fuzz.int, Fuzz.float, Fuzz.bool )) "GraphQL.Argument.list" <|
            \( string, int, float, bool ) ->
                Argument.list
                    [ Argument.string string
                    , Argument.int int
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
                        (Encode.list
                            [ Encode.string string
                            , Encode.int int
                            , Encode.float float
                            , Encode.bool bool
                            , Encode.null
                            , Encode.list
                                [ Encode.string "list"
                                , Encode.int 0
                                , Encode.float 3.14
                                , Encode.bool True
                                ]
                            , Encode.array
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
        , fuzz (Fuzz.tuple4 ( Fuzz.string, Fuzz.int, Fuzz.float, Fuzz.bool )) "GraphQL.Argument.array" <|
            \( string, int, float, bool ) ->
                [ Argument.string string
                , Argument.int int
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
                    |> Array.fromList
                    |> Argument.array
                    |> Argument.toValue
                    |> Expect.equal
                        ([ Encode.string string
                         , Encode.int int
                         , Encode.float float
                         , Encode.bool bool
                         , Encode.null
                         , Encode.list
                            [ Encode.string "list"
                            , Encode.int 0
                            , Encode.float 3.14
                            , Encode.bool True
                            ]
                         , Encode.array
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
                            |> Encode.array
                        )
        , fuzz (Fuzz.tuple4 ( Fuzz.string, Fuzz.int, Fuzz.float, Fuzz.bool )) "GraphQL.Argument.object" <|
            \( string, int, float, bool ) ->
                [ ( "string", Argument.string string )
                , ( "int", Argument.int int )
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
                         , ( "int", Encode.int int )
                         , ( "float", Encode.float float )
                         , ( "bool", Encode.bool bool )
                         , ( "null", Encode.null )
                         , ( "list"
                           , Encode.list
                                [ Encode.string "list"
                                , Encode.int 0
                                , Encode.float 3.14
                                , Encode.bool True
                                ]
                           )
                         , ( "array"
                           , Encode.array
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


selectorStructureSheet : Test
selectorStructureSheet =
    describe "Test GraphQL.Selector graph structure builder functions"
        [ test "Empty graph" <|
            \_ ->
                Selector.string
                    |> Selector.render
                    |> Expect.equal Nothing
        , test "Single graph" <|
            \_ ->
                Selector.succeed identity
                    |> Selector.field "bar" [] Selector.string
                    |> Selector.render
                    |> Expect.equal (Just "bar")
        , test "Multiple graph" <|
            \_ ->
                Selector.succeed (,,)
                    |> Selector.field "bar" [] Selector.string
                    |> Selector.field "foo" [] Selector.string
                    |> Selector.field "baz" [] Selector.string
                    |> Selector.render
                    |> Expect.equal (Just "bar foo baz")
        , test "Nested graph" <|
            \_ ->
                Selector.succeed identity
                    |> Selector.field "bar"
                        []
                        (Selector.succeed identity
                            |> Selector.field "foo"
                                []
                                (Selector.succeed identity
                                    |> Selector.field "baz" [] Selector.string
                                )
                        )
                    |> Selector.render
                    |> Expect.equal (Just "bar{foo{baz}}")
        , test "Nested multiple graph" <|
            \_ ->
                Selector.succeed (,,)
                    |> Selector.field "bar" [] Selector.string
                    |> Selector.field "foo"
                        []
                        (Selector.succeed (,,)
                            |> Selector.field "bar1" [] Selector.string
                            |> Selector.field "foo1"
                                []
                                (Selector.succeed (,,)
                                    |> Selector.field "bar2" [] Selector.string
                                    |> Selector.field "foo2" [] Selector.string
                                    |> Selector.field "baz2" [] Selector.string
                                )
                            |> Selector.field "baz1" [] Selector.string
                        )
                    |> Selector.field "baz" [] Selector.string
                    |> Selector.render
                    |> Expect.equal (Just "bar foo{bar1 foo1{bar2 foo2 baz2} baz1} baz")
        , test "Argumented graph" <|
            \_ ->
                Selector.succeed identity
                    |> Selector.field "bar"
                        [ ( "foo", Argument.string "baz" )
                        ]
                        Selector.string
                    |> Selector.render
                    |> Expect.equal (Just """bar(foo:"baz")""")
        , test "Nested argumented graph" <|
            \_ ->
                Selector.succeed identity
                    |> Selector.field "bar"
                        [ ( "foo", Argument.string "baz" )
                        ]
                        (Selector.succeed identity
                            |> Selector.field "bar1"
                                [ ( "foo1", Argument.string "baz1" )
                                ]
                                Selector.string
                        )
                    |> Selector.render
                    |> Expect.equal (Just """bar(foo:"baz"){bar1(foo1:"baz1")}""")
        , test "Aliased graph" <|
            \_ ->
                Selector.succeed identity
                    |> Selector.aliased "foo" "bar" [] Selector.string
                    |> Selector.render
                    |> Expect.equal (Just "foo:bar")
        , test "Aliased argumented graph" <|
            \_ ->
                Selector.succeed identity
                    |> Selector.aliased "foo"
                        "bar"
                        [ ( "baz", Argument.int 0 )
                        ]
                        Selector.string
                    |> Selector.render
                    |> Expect.equal (Just "foo:bar(baz:0)")
        , test "Full graph" <|
            \_ ->
                Selector.succeed (,,)
                    |> Selector.aliased
                        "bar"
                        "bar_zero"
                        [ ( "str", Argument.string "zero" )
                        , ( "int", Argument.int 0 )
                        ]
                        Selector.string
                    |> Selector.aliased
                        "foo"
                        "foo_zero"
                        [ ( "str", Argument.string "zero" )
                        , ( "int", Argument.int 0 )
                        ]
                        (Selector.succeed (,,)
                            |> Selector.aliased
                                "bar1"
                                "bar_first"
                                [ ( "str", Argument.string "first" )
                                , ( "int", Argument.int 1 )
                                ]
                                Selector.string
                            |> Selector.aliased
                                "foo1"
                                "foo_first"
                                [ ( "str", Argument.string "first" )
                                , ( "int", Argument.int 1 )
                                ]
                                (Selector.succeed (,,)
                                    |> Selector.aliased
                                        "bar2"
                                        "bar_second"
                                        [ ( "str", Argument.string "second" )
                                        , ( "int", Argument.int 2 )
                                        ]
                                        Selector.string
                                    |> Selector.aliased
                                        "foo2"
                                        "foo_second"
                                        [ ( "str", Argument.string "second" )
                                        , ( "int", Argument.int 2 )
                                        ]
                                        Selector.string
                                    |> Selector.aliased
                                        "baz2"
                                        "baz_second"
                                        [ ( "str", Argument.string "second" )
                                        , ( "int", Argument.int 2 )
                                        ]
                                        Selector.string
                                )
                            |> Selector.aliased
                                "baz1"
                                "baz_first"
                                [ ( "str", Argument.string "first" )
                                , ( "int", Argument.int 1 )
                                ]
                                Selector.string
                        )
                    |> Selector.aliased
                        "baz"
                        "baz_zero"
                        [ ( "str", Argument.string "zero" )
                        , ( "int", Argument.int 0 )
                        ]
                        Selector.string
                    |> Selector.render
                    |> Expect.equal (Just """bar:bar_zero(str:"zero",int:0) foo:foo_zero(str:"zero",int:0){bar1:bar_first(str:"first",int:1) foo1:foo_first(str:"first",int:1){bar2:bar_second(str:"second",int:2) foo2:foo_second(str:"second",int:2) baz2:baz_second(str:"second",int:2)} baz1:baz_first(str:"first",int:1)} baz:baz_zero(str:"zero",int:0)""")
        ]


selectorStringSheet : Test
selectorStringSheet =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] Selector.string
                |> Selector.field "bar" [] Selector.string
    in
    describe "Test GraphQL.Selector.string selector"
        [ test "Invalid source with direct selector" <|
            \_ ->
                """
                0
                """
                    |> Selector.decodeString Selector.string
                    |> Expect.equal (Err "Expecting a String but instead got: 0")
        , test "Valid source with direct selector" <|
            \_ ->
                """
                "string value"
                """
                    |> Selector.decodeString Selector.string
                    |> Expect.equal (Ok "string value")
        , test "Invalid source with field selector" <|
            \_ ->
                """
                {
                    "foo": false,
                    "bar": "another value"
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "Expecting a String at _.foo but instead got: false")
        , test "Valid source with field selector" <|
            \_ ->
                """
                {
                    "foo": "string value",
                    "bar": "another value"
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Ok ( "string value", "another value" ))
        , test "Build graph" <|
            \_ ->
                Selector.render Selector.string
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] Selector.string
                    |> Selector.field "bar" [] Selector.string
                    |> Selector.render
                    |> Expect.equal (Just "foo bar")
        ]


selectorBoolSheet : Test
selectorBoolSheet =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] Selector.bool
                |> Selector.field "bar" [] Selector.bool
    in
    describe "Test GraphQL.Selector.bool selector"
        [ test "Invalid source with direct selector" <|
            \_ ->
                """
                0
                """
                    |> Selector.decodeString Selector.bool
                    |> Expect.equal (Err "Expecting a Bool but instead got: 0")
        , test "Valid source with direct selector" <|
            \_ ->
                """
                false
                """
                    |> Selector.decodeString Selector.bool
                    |> Expect.equal (Ok False)
        , test "Invalid source with field selector" <|
            \_ ->
                """
                {
                    "foo": "string value",
                    "bar": true
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "Expecting a Bool at _.foo but instead got: \"string value\"")
        , test "Valid source with field selector" <|
            \_ ->
                """
                {
                    "foo": true,
                    "bar": false
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Ok ( True, False ))
        , test "Build graph" <|
            \_ ->
                Selector.render Selector.bool
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] Selector.bool
                    |> Selector.field "bar" [] Selector.bool
                    |> Selector.render
                    |> Expect.equal (Just "foo bar")
        ]


selectorIntSheet : Test
selectorIntSheet =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] Selector.int
                |> Selector.field "bar" [] Selector.int
    in
    describe "Test GraphQL.Selector.int selector"
        [ test "Invalid source with direct selector" <|
            \_ ->
                """
                0.1
                """
                    |> Selector.decodeString Selector.int
                    |> Expect.equal (Err "Expecting an Int but instead got: 0.1")
        , test "Valid source with direct selector" <|
            \_ ->
                """
                1
                """
                    |> Selector.decodeString Selector.int
                    |> Expect.equal (Ok 1)
        , test "Invalid source with field selector" <|
            \_ ->
                """
                {
                    "foo": "string value",
                    "bar": 0
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "Expecting an Int at _.foo but instead got: \"string value\"")
        , test "Valid source with field selector" <|
            \_ ->
                """
                {
                    "foo": 2,
                    "bar": 3
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Ok ( 2, 3 ))
        , test "Build graph" <|
            \_ ->
                Selector.render Selector.int
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] Selector.int
                    |> Selector.field "bar" [] Selector.int
                    |> Selector.render
                    |> Expect.equal (Just "foo bar")
        ]


selectorFloatSheet : Test
selectorFloatSheet =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] Selector.float
                |> Selector.field "bar" [] Selector.float
    in
    describe "Test GraphQL.Selector.float selector"
        [ test "Invalid source with direct selector" <|
            \_ ->
                """
                false
                """
                    |> Selector.decodeString Selector.float
                    |> Expect.equal (Err "Expecting a Float but instead got: false")
        , test "Valid source with direct selector" <|
            \_ ->
                """
                1
                """
                    |> Selector.decodeString Selector.float
                    |> Expect.equal (Ok 1)
        , test "Invalid source with field selector" <|
            \_ ->
                """
                {
                    "foo": "string value",
                    "bar": 0
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "Expecting a Float at _.foo but instead got: \"string value\"")
        , test "Valid source with field selector" <|
            \_ ->
                """
                {
                    "foo": 0,
                    "bar": 3.1
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Ok ( 0, 3.1 ))
        , test "Build graph" <|
            \_ ->
                Selector.render Selector.float
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] Selector.float
                    |> Selector.field "bar" [] Selector.float
                    |> Selector.render
                    |> Expect.equal (Just "foo bar")
        ]


selectorNullableSheet : Test
selectorNullableSheet =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.nullable Selector.string)
                |> Selector.field "bar" [] (Selector.nullable Selector.string)
    in
    describe "Test GraphQL.Selector.nullable selector"
        [ test "Invalid source with direct selector" <|
            \_ ->
                """
                true
                """
                    |> Selector.decodeString (Selector.nullable Selector.string)
                    |> Expect.equal
                        ("I ran into the following problems:\n"
                            ++ "\nExpecting null but instead got: true"
                            ++ "\nExpecting a String but instead got: true"
                            |> Err
                        )
        , test "Valid nullable source with direct selector" <|
            \_ ->
                """
                null
                """
                    |> Selector.decodeString (Selector.nullable Selector.string)
                    |> Expect.equal (Ok Nothing)
        , test "Valid source with direct selector" <|
            \_ ->
                """
                "string value"
                """
                    |> Selector.decodeString (Selector.nullable Selector.string)
                    |> Expect.equal (Ok (Just "string value"))
        , test "Invalid source with field selector" <|
            \_ ->
                """
                {
                    "foo": 0,
                    "bar": "string value"
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal
                        ("I ran into the following problems at _.foo:\n"
                            ++ "\nExpecting null but instead got: 0"
                            ++ "\nExpecting a String but instead got: 0"
                            |> Err
                        )
        , test "Valid source with field selector" <|
            \_ ->
                """
                {
                    "foo": null,
                    "bar": "string value"
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Ok ( Nothing, Just "string value" ))
        , test "Build graph" <|
            \_ ->
                Selector.render (Selector.nullable Selector.string)
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.nullable Selector.string)
                    |> Selector.field "bar" [] (Selector.nullable Selector.string)
                    |> Selector.render
                    |> Expect.equal (Just "foo bar")
        , test "Build field nested graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.nullable Selector.string)
                    |> Selector.field "bar"
                        []
                        (Selector.succeed identity
                            |> Selector.field "baz" [] Selector.string
                            |> Selector.nullable
                        )
                    |> Selector.render
                    |> Expect.equal (Just "foo bar{baz}")
        ]


selectorListSheet : Test
selectorListSheet =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.list Selector.bool)
                |> Selector.field "bar" [] (Selector.list Selector.int)
    in
    describe "Test GraphQL.Selector.list selector"
        [ test "Invalid source with direct selector" <|
            \_ ->
                """
                true
                """
                    |> Selector.decodeString (Selector.list Selector.string)
                    |> Expect.equal (Err "Expecting a List but instead got: true")
        , test "Invalid source items with direct selector" <|
            \_ ->
                """
                ["first", 0, "second"]
                """
                    |> Selector.decodeString (Selector.list Selector.string)
                    |> Expect.equal (Err "Expecting a String at _[1] but instead got: 0")
        , test "Valid source with direct selector" <|
            \_ ->
                """
                ["first", "second"]
                """
                    |> Selector.decodeString (Selector.list Selector.string)
                    |> Expect.equal (Ok [ "first", "second" ])
        , test "Invalid source with field selector" <|
            \_ ->
                """
                {
                    "foo": 0,
                    "bar": [1, 0]
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "Expecting a List at _.foo but instead got: 0")
        , test "Valid source with field selector" <|
            \_ ->
                """
                {
                    "foo": [true, false, false],
                    "bar": [1, 2, 0]
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Ok ( [ True, False, False ], [ 1, 2, 0 ] ))
        , test "Build graph" <|
            \_ ->
                Selector.render (Selector.list Selector.string)
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.list Selector.string)
                    |> Selector.field "bar" [] (Selector.list Selector.string)
                    |> Selector.render
                    |> Expect.equal (Just "foo bar")
        , test "Build field nested graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.list Selector.string)
                    |> Selector.field "bar"
                        []
                        (Selector.succeed identity
                            |> Selector.field "baz" [] Selector.string
                            |> Selector.list
                        )
                    |> Selector.render
                    |> Expect.equal (Just "foo bar{baz}")
        ]


selectorArraySheet : Test
selectorArraySheet =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.array Selector.bool)
                |> Selector.field "bar" [] (Selector.array Selector.int)
    in
    describe "Test GraphQL.Selector.array selector"
        [ test "Invalid source with direct selector" <|
            \_ ->
                """
                true
                """
                    |> Selector.decodeString (Selector.array Selector.string)
                    |> Expect.equal (Err "Expecting an Array but instead got: true")
        , test "Invalid source items with direct selector" <|
            \_ ->
                """
                ["first", 0, "second"]
                """
                    |> Selector.decodeString (Selector.array Selector.string)
                    |> Expect.equal (Err "Expecting a String at _[1] but instead got: 0")
        , test "Valid source with direct selector" <|
            \_ ->
                """
                ["first", "second"]
                """
                    |> Selector.decodeString (Selector.array Selector.string)
                    |> Expect.equal (Ok (Array.fromList [ "first", "second" ]))
        , test "Invalid source with field selector" <|
            \_ ->
                """
                {
                    "foo": 0,
                    "bar": [1, 0]
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "Expecting an Array at _.foo but instead got: 0")
        , test "Valid source with field selector" <|
            \_ ->
                """
                {
                    "foo": [true, false, false],
                    "bar": [1, 2, 0]
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal
                        (Ok
                            ( Array.fromList [ True, False, False ]
                            , Array.fromList [ 1, 2, 0 ]
                            )
                        )
        , test "Build graph" <|
            \_ ->
                Selector.render (Selector.array Selector.string)
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.array Selector.string)
                    |> Selector.field "bar" [] (Selector.array Selector.string)
                    |> Selector.render
                    |> Expect.equal (Just "foo bar")
        , test "Build field nested graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.array Selector.string)
                    |> Selector.field "bar"
                        []
                        (Selector.succeed identity
                            |> Selector.field "baz" [] Selector.string
                            |> Selector.array
                        )
                    |> Selector.render
                    |> Expect.equal (Just "foo bar{baz}")
        ]


selectorDictSheet : Test
selectorDictSheet =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.dict Selector.bool)
                |> Selector.field "bar" [] (Selector.dict Selector.int)
    in
    describe "Test GraphQL.Selector.dict selector"
        [ test "Invalid source with direct selector" <|
            \_ ->
                """
                [true]
                """
                    |> Selector.decodeString (Selector.dict Selector.string)
                    |> Expect.equal (Err "Expecting an object but instead got: [true]")
        , test "Invalid source items with direct selector" <|
            \_ ->
                """
                {
                    "key1": true,
                    "key2": "string value"
                }
                """
                    |> Selector.decodeString (Selector.dict Selector.string)
                    |> Expect.equal
                        (Err "Expecting a String at _.key1 but instead got: true")
        , test "Valid source with direct selector" <|
            \_ ->
                """
                {
                    "key1": "value 1",
                    "key2": "value 2"
                }
                """
                    |> Selector.decodeString (Selector.dict Selector.string)
                    |> Expect.equal
                        ([ ( "key1", "value 1" )
                         , ( "key2", "value 2" )
                         ]
                            |> Dict.fromList
                            |> Ok
                        )
        , test "Invalid source with field selector" <|
            \_ ->
                """
                {
                    "foo": 0,
                    "bar": {
                        "key1": 1,
                        "key2": 2
                    }
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "Expecting an object at _.foo but instead got: 0")
        , test "Valid source with field selector" <|
            \_ ->
                """
                {
                    "foo": {
                        "key1": true,
                        "key2": false
                    },
                    "bar": {
                        "key3": 3,
                        "key4": 4
                    }
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal
                        (Ok
                            ( Dict.fromList
                                [ ( "key1", True )
                                , ( "key2", False )
                                ]
                            , Dict.fromList
                                [ ( "key3", 3 )
                                , ( "key4", 4 )
                                ]
                            )
                        )
        , test "Build graph" <|
            \_ ->
                Selector.render (Selector.dict Selector.string)
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.dict Selector.string)
                    |> Selector.field "bar" [] (Selector.dict Selector.string)
                    |> Selector.render
                    |> Expect.equal (Just "foo bar")
        , test "Build field nested graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.dict Selector.string)
                    |> Selector.field "bar"
                        []
                        (Selector.succeed identity
                            |> Selector.field "baz" [] Selector.string
                            |> Selector.dict
                        )
                    |> Selector.render
                    |> Expect.equal (Just "foo bar{baz}")
        ]


selectorKeyValuePairsSheet : Test
selectorKeyValuePairsSheet =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.keyValuePairs Selector.bool)
                |> Selector.field "bar" [] (Selector.keyValuePairs Selector.int)
    in
    describe "Test GraphQL.Selector.keyValuePairs selector"
        [ test "Invalid source with direct selector" <|
            \_ ->
                """
                [true]
                """
                    |> Selector.decodeString (Selector.keyValuePairs Selector.string)
                    |> Expect.equal (Err "Expecting an object but instead got: [true]")
        , test "Invalid source items with direct selector" <|
            \_ ->
                """
                {
                    "key1": true,
                    "key2": "string value"
                }
                """
                    |> Selector.decodeString (Selector.keyValuePairs Selector.string)
                    |> Expect.equal
                        (Err "Expecting a String at _.key1 but instead got: true")
        , test "Valid source with direct selector" <|
            \_ ->
                """
                {
                    "key1": "value 1",
                    "key2": "value 2"
                }
                """
                    |> Selector.decodeString (Selector.keyValuePairs Selector.string)
                    |> Expect.equal
                        (Ok
                            [ ( "key2", "value 2" )
                            , ( "key1", "value 1" )
                            ]
                        )
        , test "Invalid source with field selector" <|
            \_ ->
                """
                {
                    "foo": 0,
                    "bar": {
                        "key1": 1,
                        "key2": 2
                    }
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "Expecting an object at _.foo but instead got: 0")
        , test "Valid source with field selector" <|
            \_ ->
                """
                {
                    "foo": {
                        "key1": true,
                        "key2": false
                    },
                    "bar": {
                        "key3": 3,
                        "key4": 4
                    }
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal
                        (Ok
                            ( [ ( "key2", False )
                              , ( "key1", True )
                              ]
                            , [ ( "key4", 4 )
                              , ( "key3", 3 )
                              ]
                            )
                        )
        , test "Build graph" <|
            \_ ->
                Selector.render (Selector.keyValuePairs Selector.string)
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.keyValuePairs Selector.string)
                    |> Selector.field "bar" [] (Selector.keyValuePairs Selector.string)
                    |> Selector.render
                    |> Expect.equal (Just "foo bar")
        , test "Build field nested graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.keyValuePairs Selector.string)
                    |> Selector.field "bar"
                        []
                        (Selector.succeed identity
                            |> Selector.field "baz" [] Selector.string
                            |> Selector.keyValuePairs
                        )
                    |> Selector.render
                    |> Expect.equal (Just "foo bar{baz}")
        ]


selectorIndexSheet : Test
selectorIndexSheet =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.index 0 Selector.int)
                |> Selector.field "bar" [] (Selector.index 1 Selector.string)
    in
    describe "Test GraphQL.Selector.index selector"
        [ test "Invalid source with direct selector" <|
            \_ ->
                """
                true
                """
                    |> Selector.decodeString (Selector.index 1 Selector.string)
                    |> Expect.equal (Err "Expecting an array but instead got: true")
        , test "Valid short source with direct selector" <|
            \_ ->
                """
                []
                """
                    |> Selector.decodeString (Selector.index 0 Selector.string)
                    |> Expect.equal
                        ("Expecting a longer array. "
                            ++ "Need index 0 but there are only 0 entries but instead got: []"
                            |> Err
                        )
        , test "Valid source with direct selector" <|
            \_ ->
                """
                [null, "string value"]
                """
                    |> Selector.decodeString (Selector.index 1 Selector.string)
                    |> Expect.equal (Ok "string value")
        , test "Invalid source with field selector" <|
            \_ ->
                """
                {
                    "foo": 0,
                    "bar": [true, "string value", null, 1]
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "Expecting an array at _.foo but instead got: 0")
        , test "Valid source with field selector" <|
            \_ ->
                """
                {
                    "foo": [0, null, "string", false],
                    "bar": [true, "string value", null, 1]
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Ok ( 0, "string value" ))
        , test "Build graph" <|
            \_ ->
                Selector.render (Selector.index 0 Selector.string)
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.index 0 Selector.string)
                    |> Selector.field "bar" [] (Selector.index 0 Selector.string)
                    |> Selector.render
                    |> Expect.equal (Just "foo bar")
        , test "Build field nested graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.index 0 Selector.string)
                    |> Selector.field "bar"
                        []
                        (Selector.succeed identity
                            |> Selector.field "baz" [] Selector.string
                            |> Selector.index 0
                        )
                    |> Selector.render
                    |> Expect.equal (Just "foo bar{baz}")
        ]


selectorMaybeSheet : Test
selectorMaybeSheet =
    let
        json =
            """
            {
                "name": "tom",
                "age": 42,
                "status": null
            }
            """
    in
    describe "Test GraphQL.Selector.maybe selector"
        [ test "Valid type of existing field" <|
            \_ ->
                Selector.succeed identity
                    |> Selector.field "age" [] (Selector.maybe Selector.int)
                    |> flip Selector.decodeString json
                    |> Expect.equal (Ok (Just 42))
        , test "Invalid type of existing field" <|
            \_ ->
                Selector.succeed identity
                    |> Selector.field "name" [] (Selector.maybe Selector.int)
                    |> flip Selector.decodeString json
                    |> Expect.equal (Ok Nothing)
        , test "Null type of existing field" <|
            \_ ->
                Selector.succeed identity
                    |> Selector.field "status" [] (Selector.maybe Selector.int)
                    |> flip Selector.decodeString json
                    |> Expect.equal (Ok Nothing)
        , test "Not existing field" <|
            \_ ->
                Selector.succeed identity
                    |> Selector.field "height" [] (Selector.maybe Selector.int)
                    |> flip Selector.decodeString json
                    |> Expect.equal (Err """Expecting an object with a field named `height` but instead got: {"name":"tom","age":42,"status":null}""")
        , test "Build graph" <|
            \_ ->
                Selector.render (Selector.maybe Selector.string)
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.maybe Selector.string)
                    |> Selector.field "bar" [] (Selector.maybe Selector.string)
                    |> Selector.render
                    |> Expect.equal (Just "foo bar")
        , test "Build field nested graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.maybe Selector.string)
                    |> Selector.field "bar"
                        []
                        (Selector.succeed identity
                            |> Selector.field "baz" [] Selector.string
                            |> Selector.maybe
                        )
                    |> Selector.render
                    |> Expect.equal (Just "foo bar{baz}")
        ]


selectorOneOfSheet : Test
selectorOneOfSheet =
    describe "Test GraphQL.Selector.oneOf selector"
        [ test "Empty list of Selectors" <|
            \_ ->
                """
                null
                """
                    |> Selector.decodeString (Selector.oneOf [])
                    |> Expect.equal (Err "I ran into the following problems:\n\n")
        , test "Single type and invalid source" <|
            \_ ->
                """
                0
                """
                    |> Selector.decodeString (Selector.oneOf [ Selector.string ])
                    |> Expect.equal
                        ("I ran into the following problems:\n"
                            ++ "\nExpecting a String but instead got: 0"
                            |> Err
                        )
        , test "Multiple type and invalid source" <|
            \_ ->
                """
                0
                """
                    |> Selector.decodeString (Selector.oneOf [ Selector.string, Selector.null "default" ])
                    |> Expect.equal
                        ("I ran into the following problems:\n"
                            ++ "\nExpecting a String but instead got: 0"
                            ++ "\nExpecting null but instead got: 0"
                            |> Err
                        )
        , test "Single type and valid source" <|
            \_ ->
                """
                0
                """
                    |> Selector.decodeString (Selector.oneOf [ Selector.int ])
                    |> Expect.equal (Ok 0)
        , test "Multiple type and valid source" <|
            \_ ->
                """
                [1, null, 2]
                """
                    |> Selector.decodeString (Selector.list (Selector.oneOf [ Selector.int, Selector.null 0 ]))
                    |> Expect.equal (Ok [ 1, 0, 2 ])
        , test "Build graph" <|
            \_ ->
                Selector.render (Selector.oneOf [ Selector.string, Selector.null "default" ])
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.oneOf [ Selector.string, Selector.null "default" ])
                    |> Selector.field "bar" [] (Selector.oneOf [ Selector.string, Selector.null "default" ])
                    |> Selector.render
                    |> Expect.equal (Just "foo bar")
        , test "Build field nested graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.oneOf [ Selector.string, Selector.null "default" ])
                    |> Selector.field "bar"
                        []
                        (Selector.oneOf
                            [ Selector.succeed identity
                                |> Selector.field "baz" [] Selector.string
                            , Selector.succeed identity
                                |> Selector.field "boo" [] Selector.string
                            ]
                        )
                    |> Selector.render
                    |> Expect.equal (Just "foo bar{baz boo}")
        ]


selectorMapSheet : Test
selectorMapSheet =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.map String.length Selector.string)
                |> Selector.field "bar" [] (Selector.map ((+) 1) Selector.int)
    in
    describe "Test GraphQL.Selector.map selector"
        [ test "Invalid source with direct selector" <|
            \_ ->
                """
                0
                """
                    |> Selector.decodeString (Selector.map String.length Selector.string)
                    |> Expect.equal (Err "Expecting a String but instead got: 0")
        , test "Valid source with direct selector" <|
            \_ ->
                """
                "string value"
                """
                    |> Selector.decodeString (Selector.map String.length Selector.string)
                    |> Expect.equal (Ok 12)
        , test "Invalid source with field selector" <|
            \_ ->
                """
                {
                    "foo": false,
                    "bar": 1
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "Expecting a String at _.foo but instead got: false")
        , test "Valid source with field selector" <|
            \_ ->
                """
                {
                    "foo": "string value",
                    "bar": 4
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Ok ( 12, 5 ))
        , test "Build graph" <|
            \_ ->
                Selector.render (Selector.map String.length Selector.string)
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.map String.length Selector.string)
                    |> Selector.field "bar" [] (Selector.map String.length Selector.string)
                    |> Selector.render
                    |> Expect.equal (Just "foo bar")
        , test "Build field nested graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.map String.length Selector.string)
                    |> Selector.field "bar"
                        []
                        (Selector.succeed identity
                            |> Selector.field "baz" [] (Selector.map String.length Selector.string)
                        )
                    |> Selector.render
                    |> Expect.equal (Just "foo bar{baz}")
        ]


selectorAndThenSheet : Test
selectorAndThenSheet =
    let
        onlyPositive : Selector number -> Selector number
        onlyPositive =
            Selector.andThen
                (\x ->
                    if x < 0 then
                        Selector.fail ("Expecting a positive number but instead got: " ++ toString x)
                    else
                        Selector.succeed x
                )

        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "bar"
                    []
                    (Selector.andThen
                        (\bar ->
                            case bar of
                                0 ->
                                    Selector.succeed (,)
                                        |> Selector.field "first" [] Selector.bool
                                        |> Selector.field "second" [] Selector.string

                                1 ->
                                    Selector.succeed ( False, "empty" )

                                _ ->
                                    Selector.fail ("Invalid bar: " ++ toString bar)
                        )
                        Selector.int
                    )
                |> Selector.field "foo" [] (onlyPositive Selector.float)
    in
    describe "Test GraphQL.Selector.andThen selector"
        [ test "Invalid source with direct selector" <|
            \_ ->
                """
                -1
                """
                    |> Selector.decodeString (onlyPositive Selector.int)
                    |> Expect.equal (Err "I ran into a `fail` decoder: Expecting a positive number but instead got: -1")
        , test "Valid source with direct selector" <|
            \_ ->
                """
                1
                """
                    |> Selector.decodeString (onlyPositive Selector.int)
                    |> Expect.equal (Ok 1)
        , test "Invalid source with field selector" <|
            \_ ->
                """
                {
                    "foo": 3.14,
                    "bar": false
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "Expecting an Int at _.bar but instead got: false")
        , test "Valid source and invalid andThen with field selector" <|
            \_ ->
                """
                {
                    "foo": 3.14,
                    "bar": 2
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "I ran into a `fail` decoder at _.bar: Invalid bar: 2")
        , test "Valid source and valid hardcoded andThen with field selector" <|
            \_ ->
                """
                {
                    "foo": 3.14,
                    "bar": 1
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Ok ( ( False, "empty" ), 3.14 ))
        , test "Invalid source and valid andThen with field selector" <|
            \_ ->
                """
                {
                    "foo": 3.14,
                    "bar": 0
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "Expecting an object with a field named `second` at _.bar but instead got: 0")
        , test "Build graph" <|
            \_ ->
                Selector.render (onlyPositive Selector.int)
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.render fieldSelector
                    |> Expect.equal (Just "bar foo")
        , test "Build field nested graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "first" [] (Selector.map String.length Selector.string)
                    |> Selector.field "second" [] fieldSelector
                    |> Selector.render
                    |> Expect.equal (Just "first second{bar foo}")
        ]


selectorSucceedSheet : Test
selectorSucceedSheet =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.succeed 1)
                |> Selector.field "bar" [] (Selector.succeed True)
    in
    describe "Test GraphQL.Selector.succeed selector"
        [ test "Direct selector" <|
            \_ ->
                """
                null
                """
                    |> Selector.decodeString (Selector.succeed "str")
                    |> Expect.equal (Ok "str")
        , test "Field selector" <|
            \_ ->
                """
                {
                    "foo": null,
                    "bar": null
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Ok ( 1, True ))
        , test "Build graph" <|
            \_ ->
                Selector.render (Selector.succeed 3.14)
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.render fieldSelector
                    |> Expect.equal (Just "foo bar")
        , test "Build field nested graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.succeed Nothing)
                    |> Selector.field "bar"
                        []
                        (Selector.succeed identity
                            |> Selector.field "baz" [] (Selector.succeed Nothing)
                        )
                    |> Selector.render
                    |> Expect.equal (Just "foo bar{baz}")
        ]


selectorFailSheet : Test
selectorFailSheet =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.fail "message foo")
                |> Selector.field "bar" [] (Selector.fail "message bar")
    in
    describe "Test GraphQL.Selector.fail selector"
        [ test "Direct selector" <|
            \_ ->
                """
                null
                """
                    |> Selector.decodeString (Selector.fail "message")
                    |> Expect.equal (Err "I ran into a `fail` decoder: message")
        , test "Field selector" <|
            \_ ->
                """
                {
                    "foo": null,
                    "bar": null
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "I ran into a `fail` decoder at _.bar: message bar")
        , test "Build graph" <|
            \_ ->
                Selector.render (Selector.fail "message")
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.render fieldSelector
                    |> Expect.equal (Just "foo bar")
        , test "Build field nested graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.fail "message foo")
                    |> Selector.field "bar"
                        []
                        (Selector.succeed identity
                            |> Selector.field "baz" [] (Selector.fail "message bar.baz")
                        )
                    |> Selector.render
                    |> Expect.equal (Just "foo bar{baz}")
        ]


selectorNullSheet : Test
selectorNullSheet =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.null False)
                |> Selector.field "bar" [] (Selector.null 1)
    in
    describe "Test GraphQL.Selector.null selector"
        [ test "Same type source with direct selector" <|
            \_ ->
                """
                42
                """
                    |> Selector.decodeString (Selector.null 42)
                    |> Expect.equal (Err "Expecting null but instead got: 42")
        , test "Different type source with direct selector" <|
            \_ ->
                """
                false
                """
                    |> Selector.decodeString (Selector.null 42)
                    |> Expect.equal (Err "Expecting null but instead got: false")
        , test "Null source with direct selector" <|
            \_ ->
                """
                null
                """
                    |> Selector.decodeString (Selector.null 42)
                    |> Expect.equal (Ok 42)
        , test "Same type source with field selector" <|
            \_ ->
                """
                {
                    "foo": false,
                    "bar": null
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "Expecting null at _.foo but instead got: false")
        , test "Different type source with field selector" <|
            \_ ->
                """
                {
                    "foo": 0,
                    "bar": null
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal (Err "Expecting null at _.foo but instead got: 0")
        , test "Valid source with field selector" <|
            \_ ->
                """
                {
                    "foo": null,
                    "bar": null
                }
                """
                    |> Selector.decodeString fieldSelector
                    |> Expect.equal
                        (Ok ( False, 1 ))
        , test "Build graph" <|
            \_ ->
                Selector.render (Selector.keyValuePairs Selector.string)
                    |> Expect.equal Nothing
        , test "Build field graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.keyValuePairs Selector.string)
                    |> Selector.field "bar" [] (Selector.keyValuePairs Selector.string)
                    |> Selector.render
                    |> Expect.equal (Just "foo bar")
        , test "Build field nested graph" <|
            \_ ->
                Selector.succeed (,)
                    |> Selector.field "foo" [] (Selector.keyValuePairs Selector.string)
                    |> Selector.field "bar"
                        []
                        (Selector.succeed identity
                            |> Selector.field "baz" [] Selector.string
                            |> Selector.keyValuePairs
                        )
                    |> Selector.render
                    |> Expect.equal (Just "foo bar{baz}")
        ]


type SearchResult
    = User String
    | Counter Int


selectorOnSheet : Test
selectorOnSheet =
    let
        singleToEmptySelector =
            Selector.succeed identity
                |> Selector.on []

        singleToSingleNonFieldSelector =
            Selector.succeed identity
                |> Selector.on
                    [ ( "User", Selector.string )
                    ]

        singleToMultipeNonFieldSelector =
            Selector.succeed identity
                |> Selector.on
                    [ ( "User", Selector.map User Selector.string )
                    , ( "Counter", Selector.map Counter Selector.int )
                    ]

        multipleEmptySelector =
            Selector.succeed (,)
                |> Selector.on []
                |> Selector.field "id" [] Selector.string

        multipleToSingleNonFieldSelector =
            Selector.succeed (,)
                |> Selector.on
                    [ ( "User", Selector.string )
                    ]
                |> Selector.field "id" [] Selector.string

        multipleToMultipeNonFieldSelector =
            Selector.succeed (,)
                |> Selector.on
                    [ ( "User", Selector.map User Selector.string )
                    , ( "Counter", Selector.map Counter Selector.int )
                    ]
                |> Selector.field "id" [] Selector.string

        singleToSingleFieldSelector =
            Selector.succeed identity
                |> Selector.on
                    [ ( "User"
                      , Selector.succeed identity
                            |> Selector.field "username" [] Selector.string
                      )
                    ]

        singleToMultipeFieldSelector =
            Selector.succeed identity
                |> Selector.on
                    [ ( "User"
                      , Selector.succeed User
                            |> Selector.field "username" [] Selector.string
                      )
                    , ( "Counter"
                      , Selector.succeed Counter
                            |> Selector.field "count" [] Selector.int
                      )
                    ]

        multipleToSingleFieldSelector =
            Selector.succeed (,)
                |> Selector.field "id" [] Selector.string
                |> Selector.on
                    [ ( "User"
                      , Selector.succeed identity
                            |> Selector.field "username" [] Selector.string
                      )
                    ]

        multipleToMultipeFieldSelector =
            Selector.succeed (,)
                |> Selector.on
                    [ ( "User"
                      , Selector.succeed User
                            |> Selector.field "username" [] Selector.string
                      )
                    , ( "Counter"
                      , Selector.succeed Counter
                            |> Selector.field "count" [] Selector.int
                      )
                    ]
                |> Selector.field "id" [] Selector.string

        nestedSelector =
            Selector.succeed (,)
                |> Selector.field "search" [] Selector.string
                |> Selector.field "results" [] (Selector.list multipleToMultipeFieldSelector)
    in
    describe "Test GraphQL.Selector.on single shape with empty `on`"
        [ test "Valid source with single to empty selector" <|
            \_ ->
                """
                {}
                """
                    |> Selector.decodeString singleToEmptySelector
                    |> Expect.equal (Err "I ran into the following problems:\n\n")
        , test "Build graph with single to empty selector" <|
            \_ ->
                Selector.render singleToEmptySelector
                    |> Expect.equal Nothing
        , test "Valid source with single to single non field selector" <|
            \_ ->
                """
                {}
                """
                    |> Selector.decodeString singleToSingleNonFieldSelector
                    |> Expect.equal (Err "I ran into the following problems:\n\n")
        , test "Build graph with single to single non field selector" <|
            \_ ->
                Selector.render singleToSingleNonFieldSelector
                    |> Expect.equal Nothing
        , test "Valid source with single to multiple non field selector" <|
            \_ ->
                """
                {}
                """
                    |> Selector.decodeString singleToMultipeNonFieldSelector
                    |> Expect.equal (Err "I ran into the following problems:\n\n")
        , test "Build graph with single to multiple non field selector" <|
            \_ ->
                Selector.render singleToMultipeNonFieldSelector
                    |> Expect.equal Nothing
        , test "Invalid source with multiple to empty selector" <|
            \_ ->
                """
                {}
                """
                    |> Selector.decodeString multipleEmptySelector
                    |> Expect.equal (Err "Expecting an object with a field named `id` but instead got: {}")
        , test "Valid source with multiple to empty selector" <|
            \_ ->
                """
                {
                    "id": "identificator"
                }
                """
                    |> Selector.decodeString multipleEmptySelector
                    |> Expect.equal (Err "I ran into the following problems:\n\n")
        , test "Build graph with multiple to empty selector" <|
            \_ ->
                Selector.render multipleEmptySelector
                    |> Expect.equal (Just "id")
        , test "Invalid source with multiple to single non field selector" <|
            \_ ->
                """
                {}
                """
                    |> Selector.decodeString multipleToSingleNonFieldSelector
                    |> Expect.equal (Err "Expecting an object with a field named `id` but instead got: {}")
        , test "Valid source with multiple to single non field selector" <|
            \_ ->
                """
                {
                    "id": "identificator"
                }
                """
                    |> Selector.decodeString multipleToSingleNonFieldSelector
                    |> Expect.equal (Err "I ran into the following problems:\n\n")
        , test "Build graph with multiple to single non field selector" <|
            \_ ->
                Selector.render multipleToSingleNonFieldSelector
                    |> Expect.equal (Just "id")
        , test "Invalid source with multiple to multiple non field selector" <|
            \_ ->
                """
                {}
                """
                    |> Selector.decodeString multipleToMultipeNonFieldSelector
                    |> Expect.equal (Err "Expecting an object with a field named `id` but instead got: {}")
        , test "Valid source with multiple to multiple non field selector" <|
            \_ ->
                """
                {
                    "id": "identificator"
                }
                """
                    |> Selector.decodeString multipleToMultipeNonFieldSelector
                    |> Expect.equal (Err "I ran into the following problems:\n\n")
        , test "Build graph with multiple to multiple non field selector" <|
            \_ ->
                Selector.render multipleToMultipeNonFieldSelector
                    |> Expect.equal (Just "id")
        , test "Invalid source with single to single field selector" <|
            \_ ->
                """
                {}
                """
                    |> Selector.decodeString singleToSingleFieldSelector
                    |> Expect.equal
                        ("I ran into the following problems:\n"
                            ++ "\nExpecting an object with a field named `username` but instead got: {}"
                            |> Err
                        )
        , test "Valid source with single to single field selector" <|
            \_ ->
                """
                {
                    "username": "Bob"
                }
                """
                    |> Selector.decodeString singleToSingleFieldSelector
                    |> Expect.equal (Ok "Bob")
        , test "Build graph with single to single field selector" <|
            \_ ->
                Selector.render singleToSingleFieldSelector
                    |> Expect.equal (Just "...on User{username}")
        , test "Invalid source with single to multiple field selector" <|
            \_ ->
                """
                {}
                """
                    |> Selector.decodeString singleToMultipeFieldSelector
                    |> Expect.equal
                        ("I ran into the following problems:\n"
                            ++ "\nExpecting an object with a field named `username` but instead got: {}"
                            ++ "\nExpecting an object with a field named `count` but instead got: {}"
                            |> Err
                        )
        , test "Valid User source with single to multiple field selector" <|
            \_ ->
                """
                {
                    "username": "Bob"
                }
                """
                    |> Selector.decodeString singleToMultipeFieldSelector
                    |> Expect.equal (Ok (User "Bob"))
        , test "Valid Counter source with single to multiple field selector" <|
            \_ ->
                """
                {
                    "count": 5
                }
                """
                    |> Selector.decodeString singleToMultipeFieldSelector
                    |> Expect.equal (Ok (Counter 5))
        , test "Build graph with single to multiple field selector" <|
            \_ ->
                Selector.render singleToMultipeFieldSelector
                    |> Expect.equal (Just "...on User{username} ...on Counter{count}")
        , test "Invalid source with multiple to single field selector" <|
            \_ ->
                """
                {}
                """
                    |> Selector.decodeString multipleToSingleFieldSelector
                    |> Expect.equal
                        ("I ran into the following problems:\n"
                            ++ "\nExpecting an object with a field named `username` but instead got: {}"
                            |> Err
                        )
        , test "Valid User source with multiple to single field selector" <|
            \_ ->
                """
                {
                    "id": "identificator",
                    "username": "Bob"
                }
                """
                    |> Selector.decodeString multipleToSingleFieldSelector
                    |> Expect.equal (Ok ( "identificator", "Bob" ))
        , test "Build graph with multiple to single field selector" <|
            \_ ->
                Selector.render multipleToSingleFieldSelector
                    |> Expect.equal (Just "id ...on User{username}")
        , test "Invalid source with multiple to multiple field selector" <|
            \_ ->
                """
                {}
                """
                    |> Selector.decodeString multipleToMultipeFieldSelector
                    |> Expect.equal (Err "Expecting an object with a field named `id` but instead got: {}")
        , test "Valid User source with multiple to multiple field selector" <|
            \_ ->
                """
                {
                    "id": "identificator",
                    "username": "Bob"
                }
                """
                    |> Selector.decodeString multipleToMultipeFieldSelector
                    |> Expect.equal (Ok ( User "Bob", "identificator" ))
        , test "Valid Counter source with multiple to multiple field selector" <|
            \_ ->
                """
                {
                    "id": "identificator",
                    "count": 5
                }
                """
                    |> Selector.decodeString multipleToMultipeFieldSelector
                    |> Expect.equal (Ok ( Counter 5, "identificator" ))
        , test "Build graph with multiple to multiple field selector" <|
            \_ ->
                Selector.render multipleToMultipeFieldSelector
                    |> Expect.equal (Just "...on User{username} ...on Counter{count} id")
        , test "Invalid source with nested selector" <|
            \_ ->
                """
                {}
                """
                    |> Selector.decodeString nestedSelector
                    |> Expect.equal (Err "Expecting an object with a field named `results` but instead got: {}")
        , test "Valid empty source with nested selector" <|
            \_ ->
                """
                {
                    "search": "foo",
                    "results": []
                }
                """
                    |> Selector.decodeString nestedSelector
                    |> Expect.equal (Ok ( "foo", [] ))
        , test "Valid mixed source with nested selector" <|
            \_ ->
                """
                {
                    "search": "bar",
                    "results": [
                        {
                            "id": "identificator1",
                            "username": "Bob"
                        },
                        {
                            "id": "identificator2",
                            "count": 5
                        },
                        {
                            "id": "identificator3",
                            "username": "Tom"
                        }
                    ]
                }
                """
                    |> Selector.decodeString nestedSelector
                    |> Expect.equal
                        (Ok
                            ( "bar"
                            , [ ( User "Bob", "identificator1" )
                              , ( Counter 5, "identificator2" )
                              , ( User "Tom", "identificator3" )
                              ]
                            )
                        )
        , test "Build graph with nested selector" <|
            \_ ->
                Selector.render nestedSelector
                    |> Expect.equal (Just "search results{...on User{username} ...on Counter{count} id}")
        ]
