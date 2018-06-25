module Tests exposing (..)

import Array
import Expect exposing (Expectation)
import Fuzz
import GraphQL.Argument as Argument
import GraphQL.Internal as Internal
import GraphQL.Selector as Selector
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
                    |> Expect.equal "{asString:\"str\",asInt:1,asFloat:3.14,asBool:true,asNull:null}"
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
                    |> Expect.equal "[\"str\",1,3.14,[true,null]]"
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
                    |> Expect.equal "[\"str\",1,3.14,[true,null]]"
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
                    |> Expect.equal (Just "bar(foo:\"baz\")")
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
                    |> Expect.equal (Just "bar(foo:\"baz\"){bar1(foo1:\"baz1\")}")
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
                    |> Expect.equal (Just "bar:bar_zero(str:\"zero\",int:0) foo:foo_zero(str:\"zero\",int:0){bar1:bar_first(str:\"first\",int:1) foo1:foo_first(str:\"first\",int:1){bar2:bar_second(str:\"second\",int:2) foo2:foo_second(str:\"second\",int:2) baz2:baz_second(str:\"second\",int:2)} baz1:baz_first(str:\"first\",int:1)} baz:baz_zero(str:\"zero\",int:0)")
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
                    |> Expect.equal
                        (Err "Expecting a List but instead got: true")
        , test "Invalid source items with direct selector" <|
            \_ ->
                """
                ["first", 0, "second"]
                """
                    |> Selector.decodeString (Selector.list Selector.string)
                    |> Expect.equal
                        (Err "Expecting a String at _[1] but instead got: 0")
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
                    |> Expect.equal
                        (Err "Expecting an Array but instead got: true")
        , test "Invalid source items with direct selector" <|
            \_ ->
                """
                ["first", 0, "second"]
                """
                    |> Selector.decodeString (Selector.array Selector.string)
                    |> Expect.equal
                        (Err "Expecting a String at _[1] but instead got: 0")
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
        ]
