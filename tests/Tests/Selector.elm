module Tests.Selector exposing (tests)

import Array
import Dict
import Expect exposing (Expectation)
import GraphQL.Argument as Argument
import GraphQL.Selector as Selector exposing (Selector)
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "GraphQL.Selector"
        [ describe "graph structure builder functions" structureTests
        , describe "GraphQL.Selector.string" stringTests
        , describe "GraphQL.Selector.bool" boolTests
        , describe "GraphQL.Selector.int" intTests
        , describe "GraphQL.Selector.float" floatTests
        , describe "GraphQL.Selector.nullable" nullableTests
        , describe "GraphQL.Selector.list" listTests
        , describe "GraphQL.Selector.array" arrayTests
        , describe "GraphQL.Selector.dict" dictTests
        , describe "GraphQL.Selector.keyValuePairs" keyValuePairsTests
        , describe "GraphQL.Selector.index" indexTests
        , describe "GraphQL.Selector.maybe" maybeTests
        , describe "GraphQL.Selector.oneOf" oneOfTests
        , describe "GraphQL.Selector.map" mapTests
        , describe "GraphQL.Selector.andThen" andThenTests
        , describe "GraphQL.Selector.succeed" succeedTests
        , describe "GraphQL.Selector.fail" failTests
        , describe "GraphQL.Selector.null" nullTests
        , describe "GraphQL.Selector.on" onTests
        ]


structureTests : List Test
structureTests =
    [ test "Empty graph" <|
        \_ ->
            Selector.string
                |> Selector.render
                |> Expect.equal ""
    , test "Single graph" <|
        \_ ->
            Selector.succeed identity
                |> Selector.field "bar" [] Selector.string
                |> Selector.render
                |> Expect.equal "bar"
    , test "Multiple graph" <|
        \_ ->
            Selector.succeed (,,)
                |> Selector.field "bar" [] Selector.string
                |> Selector.field "foo" [] Selector.string
                |> Selector.field "baz" [] Selector.string
                |> Selector.render
                |> Expect.equal "bar foo baz"
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
                |> Expect.equal "bar{foo{baz}}"
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
                |> Expect.equal "bar foo{bar1 foo1{bar2 foo2 baz2} baz1} baz"
    , test "Argumented graph" <|
        \_ ->
            Selector.succeed identity
                |> Selector.field "bar"
                    [ ( "foo", Argument.string "baz" )
                    ]
                    Selector.string
                |> Selector.render
                |> Expect.equal """bar(foo:"baz")"""
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
                |> Expect.equal """bar(foo:"baz"){bar1(foo1:"baz1")}"""
    , test "Aliased graph" <|
        \_ ->
            Selector.succeed identity
                |> Selector.aliased "foo" "bar" [] Selector.string
                |> Selector.render
                |> Expect.equal "foo:bar"
    , test "Aliased argumented graph" <|
        \_ ->
            Selector.succeed identity
                |> Selector.aliased "foo"
                    "bar"
                    [ ( "baz", Argument.int 0 )
                    ]
                    Selector.string
                |> Selector.render
                |> Expect.equal "foo:bar(baz:0)"
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
                |> Expect.equal """bar:bar_zero(str:"zero",int:0) foo:foo_zero(str:"zero",int:0){bar1:bar_first(str:"first",int:1) foo1:foo_first(str:"first",int:1){bar2:bar_second(str:"second",int:2) foo2:foo_second(str:"second",int:2) baz2:baz_second(str:"second",int:2)} baz1:baz_first(str:"first",int:1)} baz:baz_zero(str:"zero",int:0)"""
    ]


stringTests : List Test
stringTests =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] Selector.string
                |> Selector.field "bar" [] Selector.string
    in
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.succeed (,)
                |> Selector.field "foo" [] Selector.string
                |> Selector.field "bar" [] Selector.string
                |> Selector.render
                |> Expect.equal "foo bar"
    ]


boolTests : List Test
boolTests =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] Selector.bool
                |> Selector.field "bar" [] Selector.bool
    in
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.succeed (,)
                |> Selector.field "foo" [] Selector.bool
                |> Selector.field "bar" [] Selector.bool
                |> Selector.render
                |> Expect.equal "foo bar"
    ]


intTests : List Test
intTests =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] Selector.int
                |> Selector.field "bar" [] Selector.int
    in
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.succeed (,)
                |> Selector.field "foo" [] Selector.int
                |> Selector.field "bar" [] Selector.int
                |> Selector.render
                |> Expect.equal "foo bar"
    ]


floatTests : List Test
floatTests =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] Selector.float
                |> Selector.field "bar" [] Selector.float
    in
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.succeed (,)
                |> Selector.field "foo" [] Selector.float
                |> Selector.field "bar" [] Selector.float
                |> Selector.render
                |> Expect.equal "foo bar"
    ]


nullableTests : List Test
nullableTests =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.nullable Selector.string)
                |> Selector.field "bar" [] (Selector.nullable Selector.string)
    in
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.nullable Selector.string)
                |> Selector.field "bar" [] (Selector.nullable Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
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
                |> Expect.equal "foo bar{baz}"
    ]


listTests : List Test
listTests =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.list Selector.bool)
                |> Selector.field "bar" [] (Selector.list Selector.int)
    in
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.list Selector.string)
                |> Selector.field "bar" [] (Selector.list Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
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
                |> Expect.equal "foo bar{baz}"
    ]


arrayTests : List Test
arrayTests =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.array Selector.bool)
                |> Selector.field "bar" [] (Selector.array Selector.int)
    in
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.array Selector.string)
                |> Selector.field "bar" [] (Selector.array Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
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
                |> Expect.equal "foo bar{baz}"
    ]


dictTests : List Test
dictTests =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.dict Selector.bool)
                |> Selector.field "bar" [] (Selector.dict Selector.int)
    in
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.dict Selector.string)
                |> Selector.field "bar" [] (Selector.dict Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
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
                |> Expect.equal "foo bar{baz}"
    ]


keyValuePairsTests : List Test
keyValuePairsTests =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.keyValuePairs Selector.bool)
                |> Selector.field "bar" [] (Selector.keyValuePairs Selector.int)
    in
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.keyValuePairs Selector.string)
                |> Selector.field "bar" [] (Selector.keyValuePairs Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
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
                |> Expect.equal "foo bar{baz}"
    ]


indexTests : List Test
indexTests =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.index 0 Selector.int)
                |> Selector.field "bar" [] (Selector.index 1 Selector.string)
    in
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.index 0 Selector.string)
                |> Selector.field "bar" [] (Selector.index 0 Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
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
                |> Expect.equal "foo bar{baz}"
    ]


maybeTests : List Test
maybeTests =
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.maybe Selector.string)
                |> Selector.field "bar" [] (Selector.maybe Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
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
                |> Expect.equal "foo bar{baz}"
    ]


oneOfEmptyTests : List Test
oneOfEmptyTests =
    let
        selector =
            Selector.oneOf []
    in
    [ test "any source" <|
        \_ ->
            """
            "string value"
            """
                |> Selector.decodeString selector
                |> Expect.equal (Err "I ran into the following problems:\n\n")
    , test "graph" <|
        \_ ->
            Selector.render selector
                |> Expect.equal ""
    ]


oneOfSingleNonFieldTests : List Test
oneOfSingleNonFieldTests =
    let
        selector =
            Selector.oneOf
                [ Selector.string
                ]
    in
    [ test "invalid source" <|
        \_ ->
            """
            0
            """
                |> Selector.decodeString selector
                |> Expect.equal
                    ("I ran into the following problems:\n"
                        ++ "\nExpecting a String but instead got: 0"
                        |> Err
                    )
    , test "valid source" <|
        \_ ->
            """
            "string value"
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok "string value")
    , test "graph" <|
        \_ ->
            Selector.render selector
                |> Expect.equal ""
    ]


oneOfMultipleNonFieldSelector : List Test
oneOfMultipleNonFieldSelector =
    let
        selector =
            Selector.oneOf
                [ Selector.map User Selector.string
                , Selector.map Counter Selector.int
                ]
    in
    [ test "invalid source" <|
        \_ ->
            """
            {}
            """
                |> Selector.decodeString selector
                |> Expect.equal
                    ("I ran into the following problems:\n"
                        ++ "\nExpecting a String but instead got: {}"
                        ++ "\nExpecting an Int but instead got: {}"
                        |> Err
                    )
    , test "valid `User` source" <|
        \_ ->
            """
            "identificator"
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok (User "identificator"))
    , test "valid `Counter` source" <|
        \_ ->
            """
            2
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok (Counter 2))
    , test "graph" <|
        \_ ->
            Selector.render selector
                |> Expect.equal ""
    ]


oneOfSingleFieldTests : List Test
oneOfSingleFieldTests =
    let
        selector =
            Selector.oneOf
                [ Selector.field "username" [] Selector.string Selector.singleton
                ]
    in
    [ test "invalid source" <|
        \_ ->
            """
            {}
            """
                |> Selector.decodeString selector
                |> Expect.equal
                    ("I ran into the following problems:\n"
                        ++ "\nExpecting an object with a field named `username` but instead got: {}"
                        |> Err
                    )
    , test "valid source" <|
        \_ ->
            """
            {
                "username": "Bob"
            }
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok "Bob")
    , test "graph" <|
        \_ ->
            Selector.render selector
                |> Expect.equal "username"
    ]


oneOfMultipleFieldSelector : List Test
oneOfMultipleFieldSelector =
    let
        selector =
            Selector.oneOf
                [ Selector.succeed User
                    |> Selector.field "username" [] Selector.string
                , Selector.succeed Counter
                    |> Selector.field "count" [] Selector.int
                ]
    in
    [ test "invalid source" <|
        \_ ->
            """
            {}
            """
                |> Selector.decodeString selector
                |> Expect.equal
                    ("I ran into the following problems:\n"
                        ++ "\nExpecting an object with a field named `username` but instead got: {}"
                        ++ "\nExpecting an object with a field named `count` but instead got: {}"
                        |> Err
                    )
    , test "valid `User` source" <|
        \_ ->
            """
            {
                "username": "Bob"
            }
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok (User "Bob"))
    , test "valid `Counter` source" <|
        \_ ->
            """
            {
                "count": 5
            }
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok (Counter 5))
    , test "graph" <|
        \_ ->
            Selector.render selector
                |> Expect.equal "username count"
    ]


oneOfNestedTests : List Test
oneOfNestedTests =
    let
        nestedSelector =
            Selector.succeed (,)
                |> Selector.field "search" [] Selector.string
                |> Selector.field "results"
                    []
                    (Selector.list
                        (Selector.oneOf
                            [ Selector.succeed (,)
                                |> Selector.field "username" [] (Selector.map User Selector.string)
                                |> Selector.field "id" [] Selector.string
                            , Selector.succeed (,)
                                |> Selector.field "count" [] (Selector.map Counter Selector.int)
                                |> Selector.field "id" [] Selector.string
                            ]
                        )
                    )
    in
    [ test "invalid source" <|
        \_ ->
            """
            {}
            """
                |> Selector.decodeString nestedSelector
                |> Expect.equal (Err "Expecting an object with a field named `results` but instead got: {}")
    , test "valid empty source" <|
        \_ ->
            """
            {
                "search": "foo",
                "results": []
            }
            """
                |> Selector.decodeString nestedSelector
                |> Expect.equal (Ok ( "foo", [] ))
    , test "valid mixed source" <|
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
    , test "graph" <|
        \_ ->
            Selector.render nestedSelector
                |> Expect.equal "search results{username id count id}"
    ]


oneOfTests : List Test
oneOfTests =
    [ describe "GraphQL.Selector.oneOf with empty selector" oneOfEmptyTests
    , describe "GraphQL.Selector.oneOf with singleNonField selector" oneOfSingleNonFieldTests
    , describe "GraphQL.Selector.oneOf with multipleNonField selector" oneOfMultipleNonFieldSelector
    , describe "GraphQL.Selector.oneOf with singleField selector" oneOfSingleFieldTests
    , describe "GraphQL.Selector.oneOf with multipleField selector" oneOfMultipleFieldSelector
    , describe "GraphQL.Selector.oneOf with nested selector" oneOfNestedTests
    ]


mapTests : List Test
mapTests =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.map String.length Selector.string)
                |> Selector.field "bar" [] (Selector.map ((+) 1) Selector.int)
    in
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.map String.length Selector.string)
                |> Selector.field "bar" [] (Selector.map String.length Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
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
                |> Expect.equal "foo bar{baz}"
    ]


andThenTests : List Test
andThenTests =
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.render fieldSelector
                |> Expect.equal "bar foo"
    , test "Build field nested graph" <|
        \_ ->
            Selector.succeed (,)
                |> Selector.field "first" [] (Selector.map String.length Selector.string)
                |> Selector.field "second" [] fieldSelector
                |> Selector.render
                |> Expect.equal "first second{bar foo}"
    ]


succeedTests : List Test
succeedTests =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.succeed 1)
                |> Selector.field "bar" [] (Selector.succeed True)
    in
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.render fieldSelector
                |> Expect.equal "foo bar"
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
                |> Expect.equal "foo bar{baz}"
    ]


failTests : List Test
failTests =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.fail "message foo")
                |> Selector.field "bar" [] (Selector.fail "message bar")
    in
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.render fieldSelector
                |> Expect.equal "foo bar"
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
                |> Expect.equal "foo bar{baz}"
    ]


nullTests : List Test
nullTests =
    let
        fieldSelector =
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.null False)
                |> Selector.field "bar" [] (Selector.null 1)
    in
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
                |> Expect.equal ""
    , test "Build field graph" <|
        \_ ->
            Selector.succeed (,)
                |> Selector.field "foo" [] (Selector.keyValuePairs Selector.string)
                |> Selector.field "bar" [] (Selector.keyValuePairs Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
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
                |> Expect.equal "foo bar{baz}"
    ]


type SearchResult
    = User String
    | Counter Int


onTests : List Test
onTests =
    let
        emptySelector =
            Selector.succeed identity
                |> Selector.on []

        singleNonFieldSelector =
            Selector.succeed identity
                |> Selector.on
                    [ ( "User", Selector.string )
                    ]

        multipeNonFieldSelector =
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

        singleFieldSelector =
            Selector.succeed identity
                |> Selector.on
                    [ ( "User"
                      , Selector.succeed identity
                            |> Selector.field "username" [] Selector.string
                      )
                    ]

        multipeFieldSelector =
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
    [ test "Valid source with single to empty selector" <|
        \_ ->
            """
                {}
                """
                |> Selector.decodeString emptySelector
                |> Expect.equal (Err "I ran into the following problems:\n\n")
    , test "Build graph with single to empty selector" <|
        \_ ->
            Selector.render emptySelector
                |> Expect.equal ""
    , test "Valid source with single to single non field selector" <|
        \_ ->
            """
                {}
                """
                |> Selector.decodeString singleNonFieldSelector
                |> Expect.equal (Err "I ran into the following problems:\n\n")
    , test "Build graph with single to single non field selector" <|
        \_ ->
            Selector.render singleNonFieldSelector
                |> Expect.equal ""
    , test "Valid source with single to multiple non field selector" <|
        \_ ->
            """
                {}
                """
                |> Selector.decodeString multipeNonFieldSelector
                |> Expect.equal (Err "I ran into the following problems:\n\n")
    , test "Build graph with single to multiple non field selector" <|
        \_ ->
            Selector.render multipeNonFieldSelector
                |> Expect.equal ""
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
                |> Expect.equal "id"
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
                |> Expect.equal "id"
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
                |> Expect.equal "id"
    , test "Invalid source with single to single field selector" <|
        \_ ->
            """
                {}
                """
                |> Selector.decodeString singleFieldSelector
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
                |> Selector.decodeString singleFieldSelector
                |> Expect.equal (Ok "Bob")
    , test "Build graph with single to single field selector" <|
        \_ ->
            Selector.render singleFieldSelector
                |> Expect.equal "...on User{username}"
    , test "Invalid source with single to multiple field selector" <|
        \_ ->
            """
                {}
                """
                |> Selector.decodeString multipeFieldSelector
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
                |> Selector.decodeString multipeFieldSelector
                |> Expect.equal (Ok (User "Bob"))
    , test "Valid Counter source with single to multiple field selector" <|
        \_ ->
            """
                {
                    "count": 5
                }
                """
                |> Selector.decodeString multipeFieldSelector
                |> Expect.equal (Ok (Counter 5))
    , test "Build graph with single to multiple field selector" <|
        \_ ->
            Selector.render multipeFieldSelector
                |> Expect.equal "...on User{username} ...on Counter{count}"
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
                |> Expect.equal "id ...on User{username}"
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
                |> Expect.equal "...on User{username} ...on Counter{count} id"
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
                |> Expect.equal "search results{...on User{username} ...on Counter{count} id}"
    ]
