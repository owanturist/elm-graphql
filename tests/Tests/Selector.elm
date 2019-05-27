module Tests.Selector exposing (tests)

import Array
import Dict
import Expect exposing (Expectation)
import GraphQL.Argument as Argument
import GraphQL.Selector as Selector exposing (Error(..), Selector)
import Json.Encode as Json
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
        , describe "GraphQL.Selector.at" atTests
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


tuple2 : a -> b -> ( a, b )
tuple2 a b =
    ( a, b )


tuple3 : a -> b -> c -> ( a, b, c )
tuple3 a b c =
    ( a, b, c )


structureTests : List Test
structureTests =
    [ test "Empty graph" <|
        \_ ->
            Selector.string
                |> Selector.render
                |> Expect.equal ""
    , test "single graph" <|
        \_ ->
            Selector.field "bar" [] Selector.string
                |> Selector.render
                |> Expect.equal "bar"
    , test "multiple graph" <|
        \_ ->
            Selector.succeed tuple3
                |> Selector.select "bar" [] Selector.string
                |> Selector.select "foo" [] Selector.string
                |> Selector.select "baz" [] Selector.string
                |> Selector.render
                |> Expect.equal "bar foo baz"
    , test "nested graph" <|
        \_ ->
            Selector.string
                |> Selector.field "baz" []
                |> Selector.field "foo" []
                |> Selector.field "bar" []
                |> Selector.render
                |> Expect.equal "bar{foo{baz}}"
    , test "nested multiple graph" <|
        \_ ->
            Selector.succeed tuple3
                |> Selector.select "bar" [] Selector.string
                |> Selector.select "foo"
                    []
                    (Selector.succeed tuple3
                        |> Selector.select "bar1" [] Selector.string
                        |> Selector.select "foo1"
                            []
                            (Selector.succeed tuple3
                                |> Selector.select "bar2" [] Selector.string
                                |> Selector.select "foo2" [] Selector.string
                                |> Selector.select "baz2" [] Selector.string
                            )
                        |> Selector.select "baz1" [] Selector.string
                    )
                |> Selector.select "baz" [] Selector.string
                |> Selector.render
                |> Expect.equal "bar foo{bar1 foo1{bar2 foo2 baz2} baz1} baz"
    , test "argumented graph" <|
        \_ ->
            Selector.field "bar"
                [ ( "foo", Argument.string "baz" )
                ]
                Selector.string
                |> Selector.render
                |> Expect.equal """bar(foo:"baz")"""
    , test "nested argumented graph" <|
        \_ ->
            Selector.field "bar"
                [ ( "foo", Argument.string "baz" )
                ]
                (Selector.field "bar1"
                    [ ( "foo1", Argument.string "baz1" )
                    ]
                    Selector.string
                )
                |> Selector.render
                |> Expect.equal """bar(foo:"baz"){bar1(foo1:"baz1")}"""
    , test "aliased graph" <|
        \_ ->
            Selector.fieldWithAlias "foo" "bar" [] Selector.string
                |> Selector.render
                |> Expect.equal "foo:bar"
    , test "aliased argumented graph" <|
        \_ ->
            Selector.fieldWithAlias "foo"
                "bar"
                [ ( "baz", Argument.int 0 )
                ]
                Selector.string
                |> Selector.render
                |> Expect.equal "foo:bar(baz:0)"
    , test "full graph" <|
        \_ ->
            Selector.succeed tuple3
                |> Selector.selectWithAlias
                    "bar"
                    "bar_zero"
                    [ ( "str", Argument.string "zero" )
                    , ( "int", Argument.int 0 )
                    ]
                    Selector.string
                |> Selector.selectWithAlias
                    "foo"
                    "foo_zero"
                    [ ( "str", Argument.string "zero" )
                    , ( "int", Argument.int 0 )
                    ]
                    (Selector.succeed tuple3
                        |> Selector.selectWithAlias
                            "bar1"
                            "bar_first"
                            [ ( "str", Argument.string "first" )
                            , ( "int", Argument.int 1 )
                            ]
                            Selector.string
                        |> Selector.selectWithAlias
                            "foo1"
                            "foo_first"
                            [ ( "str", Argument.string "first" )
                            , ( "int", Argument.int 1 )
                            ]
                            (Selector.succeed tuple3
                                |> Selector.selectWithAlias
                                    "bar2"
                                    "bar_second"
                                    [ ( "str", Argument.string "second" )
                                    , ( "int", Argument.int 2 )
                                    ]
                                    Selector.string
                                |> Selector.selectWithAlias
                                    "foo2"
                                    "foo_second"
                                    [ ( "str", Argument.string "second" )
                                    , ( "int", Argument.int 2 )
                                    ]
                                    Selector.string
                                |> Selector.selectWithAlias
                                    "baz2"
                                    "baz_second"
                                    [ ( "str", Argument.string "second" )
                                    , ( "int", Argument.int 2 )
                                    ]
                                    Selector.string
                            )
                        |> Selector.selectWithAlias
                            "baz1"
                            "baz_first"
                            [ ( "str", Argument.string "first" )
                            , ( "int", Argument.int 1 )
                            ]
                            Selector.string
                    )
                |> Selector.selectWithAlias
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
            Selector.succeed tuple2
                |> Selector.select "foo" [] Selector.string
                |> Selector.select "bar" [] Selector.string
    in
    [ test "Invalid source with direct selector" <|
        \_ ->
            """
            0
            """
                |> Selector.decodeString Selector.string
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "0\n"
                        ++ "\n"
                        ++ "Expecting a STRING"
                        |> Err
                    )
    , test "valid source with direct selector" <|
        \_ ->
            """
            "string value"
            """
                |> Selector.decodeString Selector.string
                |> Expect.equal (Ok "string value")
    , test "invalid source with field selector" <|
        \_ ->
            """
            {
                "foo": false,
                "bar": "another value"
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.foo:\n"
                        ++ "\n"
                        ++ "    false\n"
                        ++ "\n"
                        ++ "Expecting a STRING"
                        |> Err
                    )
    , test "valid source with field selector" <|
        \_ ->
            """
            {
                "foo": "string value",
                "bar": "another value"
            }
            """
                |> Selector.decodeString fieldSelector
                |> Expect.equal (Ok ( "string value", "another value" ))
    , test "build graph" <|
        \_ ->
            Selector.render Selector.string
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] Selector.string
                |> Selector.select "bar" [] Selector.string
                |> Selector.render
                |> Expect.equal "foo bar"
    ]


boolTests : List Test
boolTests =
    let
        fieldSelector =
            Selector.succeed tuple2
                |> Selector.select "foo" [] Selector.bool
                |> Selector.select "bar" [] Selector.bool
    in
    [ test "Invalid source with direct selector" <|
        \_ ->
            """
            0
            """
                |> Selector.decodeString Selector.bool
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "0\n"
                        ++ "\n"
                        ++ "Expecting a BOOL"
                        |> Err
                    )
    , test "valid source with direct selector" <|
        \_ ->
            """
            false
            """
                |> Selector.decodeString Selector.bool
                |> Expect.equal (Ok False)
    , test "invalid source with field selector" <|
        \_ ->
            """
            {
                "foo": "string value",
                "bar": true
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.foo:\n"
                        ++ "\n"
                        ++ "    \"string value\"\n"
                        ++ "\n"
                        ++ "Expecting a BOOL"
                        |> Err
                    )
    , test "valid source with field selector" <|
        \_ ->
            """
            {
                "foo": true,
                "bar": false
            }
            """
                |> Selector.decodeString fieldSelector
                |> Expect.equal (Ok ( True, False ))
    , test "build graph" <|
        \_ ->
            Selector.render Selector.bool
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] Selector.bool
                |> Selector.select "bar" [] Selector.bool
                |> Selector.render
                |> Expect.equal "foo bar"
    ]


intTests : List Test
intTests =
    let
        fieldSelector =
            Selector.succeed tuple2
                |> Selector.select "foo" [] Selector.int
                |> Selector.select "bar" [] Selector.int
    in
    [ test "Invalid source with direct selector" <|
        \_ ->
            """
            0.1
            """
                |> Selector.decodeString Selector.int
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "0.1\n"
                        ++ "\n"
                        ++ "Expecting an INT"
                        |> Err
                    )
    , test "valid source with direct selector" <|
        \_ ->
            """
            1
            """
                |> Selector.decodeString Selector.int
                |> Expect.equal (Ok 1)
    , test "invalid source with field selector" <|
        \_ ->
            """
            {
                "foo": "string value",
                "bar": 0
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.foo:\n"
                        ++ "\n"
                        ++ "    \"string value\"\n"
                        ++ "\n"
                        ++ "Expecting an INT"
                        |> Err
                    )
    , test "valid source with field selector" <|
        \_ ->
            """
            {
                "foo": 2,
                "bar": 3
            }
            """
                |> Selector.decodeString fieldSelector
                |> Expect.equal (Ok ( 2, 3 ))
    , test "build graph" <|
        \_ ->
            Selector.render Selector.int
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] Selector.int
                |> Selector.select "bar" [] Selector.int
                |> Selector.render
                |> Expect.equal "foo bar"
    ]


floatTests : List Test
floatTests =
    let
        fieldSelector =
            Selector.succeed tuple2
                |> Selector.select "foo" [] Selector.float
                |> Selector.select "bar" [] Selector.float
    in
    [ test "Invalid source with direct selector" <|
        \_ ->
            """
            false
            """
                |> Selector.decodeString Selector.float
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "false\n"
                        ++ "\n"
                        ++ "Expecting a FLOAT"
                        |> Err
                    )
    , test "valid source with direct selector" <|
        \_ ->
            """
            1
            """
                |> Selector.decodeString Selector.float
                |> Expect.equal (Ok 1)
    , test "invalid source with field selector" <|
        \_ ->
            """
            {
                "foo": "string value",
                "bar": 0
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.foo:\n"
                        ++ "\n"
                        ++ "    \"string value\"\n"
                        ++ "\n"
                        ++ "Expecting a FLOAT"
                        |> Err
                    )
    , test "valid source with field selector" <|
        \_ ->
            """
            {
                "foo": 0,
                "bar": 3.1
            }
            """
                |> Selector.decodeString fieldSelector
                |> Expect.equal (Ok ( 0, 3.1 ))
    , test "build graph" <|
        \_ ->
            Selector.render Selector.float
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] Selector.float
                |> Selector.select "bar" [] Selector.float
                |> Selector.render
                |> Expect.equal "foo bar"
    ]


nullableTests : List Test
nullableTests =
    let
        fieldSelector =
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.nullable Selector.string)
                |> Selector.select "bar" [] (Selector.nullable Selector.string)
    in
    [ test "Invalid source with direct selector" <|
        \_ ->
            """
            true
            """
                |> Selector.decodeString (Selector.nullable Selector.string)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Json.Decode.oneOf failed in the following 2 ways:\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "(1) Problem with the given value:\n"
                        ++ "    \n"
                        ++ "    true\n"
                        ++ "    \n"
                        ++ "    Expecting null\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "(2) Problem with the given value:\n"
                        ++ "    \n"
                        ++ "    true\n"
                        ++ "    \n"
                        ++ "    Expecting a STRING"
                        |> Err
                    )
    , test "valid nullable source with direct selector" <|
        \_ ->
            """
            null
            """
                |> Selector.decodeString (Selector.nullable Selector.string)
                |> Expect.equal (Ok Nothing)
    , test "valid source with direct selector" <|
        \_ ->
            """
            "string value"
            """
                |> Selector.decodeString (Selector.nullable Selector.string)
                |> Expect.equal (Ok (Just "string value"))
    , test "invalid source with field selector" <|
        \_ ->
            """
            {
                "foo": 0,
                "bar": "string value"
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("The Json.Decode.oneOf at json.foo failed in the following 2 ways:\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "(1) Problem with the given value:\n"
                        ++ "    \n"
                        ++ "    0\n"
                        ++ "    \n"
                        ++ "    Expecting null\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "(2) Problem with the given value:\n"
                        ++ "    \n"
                        ++ "    0\n"
                        ++ "    \n"
                        ++ "    Expecting a STRING"
                        |> Err
                    )
    , test "valid source with field selector" <|
        \_ ->
            """
            {
                "foo": null,
                "bar": "string value"
            }
            """
                |> Selector.decodeString fieldSelector
                |> Expect.equal (Ok ( Nothing, Just "string value" ))
    , test "build graph" <|
        \_ ->
            Selector.render (Selector.nullable Selector.string)
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.nullable Selector.string)
                |> Selector.select "bar" [] (Selector.nullable Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
    , test "build field nested graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.nullable Selector.string)
                |> Selector.select "bar" [] (Selector.field "baz" [] (Selector.nullable Selector.string))
                |> Selector.render
                |> Expect.equal "foo bar{baz}"
    ]


listTests : List Test
listTests =
    let
        fieldSelector =
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.list Selector.bool)
                |> Selector.select "bar" [] (Selector.list Selector.int)
    in
    [ test "Invalid source with direct selector" <|
        \_ ->
            """
            true
            """
                |> Selector.decodeString (Selector.list Selector.string)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "true\n"
                        ++ "\n"
                        ++ "Expecting a LIST"
                        |> Err
                    )
    , test "invalid source items with direct selector" <|
        \_ ->
            """
            ["first", 0, "second"]
            """
                |> Selector.decodeString (Selector.list Selector.string)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json[1]:\n"
                        ++ "\n"
                        ++ "    0\n"
                        ++ "\n"
                        ++ "Expecting a STRING"
                        |> Err
                    )
    , test "valid source with direct selector" <|
        \_ ->
            """
            ["first", "second"]
            """
                |> Selector.decodeString (Selector.list Selector.string)
                |> Expect.equal (Ok [ "first", "second" ])
    , test "invalid source with field selector" <|
        \_ ->
            """
            {
                "foo": 0,
                "bar": [1, 0]
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.foo:\n"
                        ++ "\n"
                        ++ "    0\n"
                        ++ "\n"
                        ++ "Expecting a LIST"
                        |> Err
                    )
    , test "valid source with field selector" <|
        \_ ->
            """
            {
                "foo": [true, false, false],
                "bar": [1, 2, 0]
            }
            """
                |> Selector.decodeString fieldSelector
                |> Expect.equal (Ok ( [ True, False, False ], [ 1, 2, 0 ] ))
    , test "build graph" <|
        \_ ->
            Selector.render (Selector.list Selector.string)
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.list Selector.string)
                |> Selector.select "bar" [] (Selector.list Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
    , test "build field nested graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.list Selector.string)
                |> Selector.select "bar" [] (Selector.field "baz" [] (Selector.list Selector.string))
                |> Selector.render
                |> Expect.equal "foo bar{baz}"
    ]


arrayTests : List Test
arrayTests =
    let
        fieldSelector =
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.array Selector.bool)
                |> Selector.select "bar" [] (Selector.array Selector.int)
    in
    [ test "Invalid source with direct selector" <|
        \_ ->
            """
            true
            """
                |> Selector.decodeString (Selector.array Selector.string)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "true\n"
                        ++ "\n"
                        ++ "Expecting an ARRAY"
                        |> Err
                    )
    , test "invalid source items with direct selector" <|
        \_ ->
            """
            ["first", 0, "second"]
            """
                |> Selector.decodeString (Selector.array Selector.string)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json[1]:\n"
                        ++ "\n"
                        ++ "    0\n"
                        ++ "\n"
                        ++ "Expecting a STRING"
                        |> Err
                    )
    , test "valid source with direct selector" <|
        \_ ->
            """
            ["first", "second"]
            """
                |> Selector.decodeString (Selector.array Selector.string)
                |> Expect.equal (Ok (Array.fromList [ "first", "second" ]))
    , test "invalid source with field selector" <|
        \_ ->
            """
            {
                "foo": 0,
                "bar": [1, 0]
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.foo:\n"
                        ++ "\n"
                        ++ "    0\n"
                        ++ "\n"
                        ++ "Expecting an ARRAY"
                        |> Err
                    )
    , test "valid source with field selector" <|
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
    , test "build graph" <|
        \_ ->
            Selector.render (Selector.array Selector.string)
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.array Selector.string)
                |> Selector.select "bar" [] (Selector.array Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
    , test "build field nested graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.array Selector.string)
                |> Selector.select "bar" [] (Selector.field "baz" [] (Selector.array Selector.string))
                |> Selector.render
                |> Expect.equal "foo bar{baz}"
    ]


dictTests : List Test
dictTests =
    let
        fieldSelector =
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.dict Selector.bool)
                |> Selector.select "bar" [] (Selector.dict Selector.int)
    in
    [ test "Invalid source with direct selector" <|
        \_ ->
            """
            [true]
            """
                |> Selector.decodeString (Selector.dict Selector.string)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "[\n"
                        ++ "        true\n"
                        ++ "    ]\n"
                        ++ "\n"
                        ++ "Expecting an OBJECT"
                        |> Err
                    )
    , test "invalid source items with direct selector" <|
        \_ ->
            """
            {
                "key1": true,
                "key2": "string value"
            }
            """
                |> Selector.decodeString (Selector.dict Selector.string)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.key1:\n"
                        ++ "\n"
                        ++ "    true\n"
                        ++ "\n"
                        ++ "Expecting a STRING"
                        |> Err
                    )
    , test "valid source with direct selector" <|
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
    , test "invalid source with field selector" <|
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
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.foo:\n"
                        ++ "\n"
                        ++ "    0\n"
                        ++ "\n"
                        ++ "Expecting an OBJECT"
                        |> Err
                    )
    , test "valid source with field selector" <|
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
    , test "build graph" <|
        \_ ->
            Selector.render (Selector.dict Selector.string)
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.dict Selector.string)
                |> Selector.select "bar" [] (Selector.dict Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
    , test "build field nested graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.dict Selector.string)
                |> Selector.select "bar" [] (Selector.field "baz" [] (Selector.dict Selector.string))
                |> Selector.render
                |> Expect.equal "foo bar{baz}"
    ]


keyValuePairsTests : List Test
keyValuePairsTests =
    let
        fieldSelector =
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.keyValuePairs Selector.bool)
                |> Selector.select "bar" [] (Selector.keyValuePairs Selector.int)
    in
    [ test "Invalid source with direct selector" <|
        \_ ->
            """
            [true]
            """
                |> Selector.decodeString (Selector.keyValuePairs Selector.string)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "[\n"
                        ++ "        true\n"
                        ++ "    ]\n"
                        ++ "\n"
                        ++ "Expecting an OBJECT"
                        |> Err
                    )
    , test "invalid source items with direct selector" <|
        \_ ->
            """
            {
                "key1": true,
                "key2": "string value"
            }
            """
                |> Selector.decodeString (Selector.keyValuePairs Selector.string)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.key1:\n"
                        ++ "\n"
                        ++ "    true\n"
                        ++ "\n"
                        ++ "Expecting a STRING"
                        |> Err
                    )
    , test "valid source with direct selector" <|
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
                        [ ( "key1", "value 1" )
                        , ( "key2", "value 2" )
                        ]
                    )
    , test "invalid source with field selector" <|
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
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.foo:\n"
                        ++ "\n"
                        ++ "    0\n"
                        ++ "\n"
                        ++ "Expecting an OBJECT"
                        |> Err
                    )
    , test "valid source with field selector" <|
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
                        ( [ ( "key1", True )
                          , ( "key2", False )
                          ]
                        , [ ( "key3", 3 )
                          , ( "key4", 4 )
                          ]
                        )
                    )
    , test "build graph" <|
        \_ ->
            Selector.render (Selector.keyValuePairs Selector.string)
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.keyValuePairs Selector.string)
                |> Selector.select "bar" [] (Selector.keyValuePairs Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
    , test "build field nested graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.keyValuePairs Selector.string)
                |> Selector.select "bar" [] (Selector.field "baz" [] (Selector.keyValuePairs Selector.string))
                |> Selector.render
                |> Expect.equal "foo bar{baz}"
    ]


atEmptyPathTests : List Test
atEmptyPathTests =
    let
        selector =
            Selector.at [] Selector.bool
    in
    [ test "invalid source with empty path" <|
        \_ ->
            """
            0
            """
                |> Selector.decodeString selector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "0\n"
                        ++ "\n"
                        ++ "Expecting a BOOL"
                        |> Err
                    )
    , test "valid source with empty path" <|
        \_ ->
            """
            true
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok True)
    , test "graph" <|
        \_ ->
            selector
                |> Selector.render
                |> Expect.equal ""
    ]


atSingletonPathTests : List Test
atSingletonPathTests =
    let
        selector =
            Selector.at [ "foo" ] Selector.bool
    in
    [ test "invalid source with empty path" <|
        \_ ->
            """
            {
                "foo": 0
            }
            """
                |> Selector.decodeString selector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.foo:\n"
                        ++ "\n"
                        ++ "    0\n"
                        ++ "\n"
                        ++ "Expecting a BOOL"
                        |> Err
                    )
    , test "valid source with empty path" <|
        \_ ->
            """
            {
                "foo": true
            }
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok True)
    , test "graph" <|
        \_ ->
            selector
                |> Selector.render
                |> Expect.equal "foo"
    ]


atLongPathTests : List Test
atLongPathTests =
    let
        selector =
            Selector.at [ "foo", "bar" ] Selector.bool
    in
    [ test "invalid source with empty path" <|
        \_ ->
            """
            {
                "foo": {
                    "bar": 0
                }
            }
            """
                |> Selector.decodeString selector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.foo.bar:\n"
                        ++ "\n"
                        ++ "    0\n"
                        ++ "\n"
                        ++ "Expecting a BOOL"
                        |> Err
                    )
    , test "valid source with empty path" <|
        \_ ->
            """
            {
                "foo": {
                    "bar": true
                }
            }
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok True)
    , test "graph" <|
        \_ ->
            selector
                |> Selector.render
                |> Expect.equal "foo{bar}"
    ]


atTests : List Test
atTests =
    [ describe "empty path" atEmptyPathTests
    , describe "singleton path" atSingletonPathTests
    , describe "long path" atLongPathTests
    ]


indexTests : List Test
indexTests =
    let
        fieldSelector =
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.index 0 Selector.int)
                |> Selector.select "bar" [] (Selector.index 1 Selector.string)
    in
    [ test "Invalid source with direct selector" <|
        \_ ->
            """
            true
            """
                |> Selector.decodeString (Selector.index 1 Selector.string)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "true\n"
                        ++ "\n"
                        ++ "Expecting an ARRAY"
                        |> Err
                    )
    , test "valid short source with direct selector" <|
        \_ ->
            """
            []
            """
                |> Selector.decodeString (Selector.index 0 Selector.string)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "[]\n"
                        ++ "\n"
                        ++ "Expecting a LONGER array. Need index 0 but only see 0 entries"
                        |> Err
                    )
    , test "valid source with direct selector" <|
        \_ ->
            """
            [null, "string value"]
            """
                |> Selector.decodeString (Selector.index 1 Selector.string)
                |> Expect.equal (Ok "string value")
    , test "invalid source with field selector" <|
        \_ ->
            """
            {
                "foo": 0,
                "bar": [true, "string value", null, 1]
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.foo:\n"
                        ++ "\n"
                        ++ "    0\n"
                        ++ "\n"
                        ++ "Expecting an ARRAY"
                        |> Err
                    )
    , test "valid source with field selector" <|
        \_ ->
            """
            {
                "foo": [0, null, "string", false],
                "bar": [true, "string value", null, 1]
            }
            """
                |> Selector.decodeString fieldSelector
                |> Expect.equal (Ok ( 0, "string value" ))
    , test "build graph" <|
        \_ ->
            Selector.render (Selector.index 0 Selector.string)
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.index 0 Selector.string)
                |> Selector.select "bar" [] (Selector.index 0 Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
    , test "build field nested graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.index 0 Selector.string)
                |> Selector.select "bar" [] (Selector.field "baz" [] (Selector.index 0 Selector.string))
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
            Selector.decodeString
                (Selector.field "age" [] (Selector.maybe Selector.int))
                json
                |> Expect.equal (Ok (Just 42))
    , test "invalid type of existing field" <|
        \_ ->
            Selector.decodeString
                (Selector.field "name" [] (Selector.maybe Selector.int))
                json
                |> Expect.equal (Ok Nothing)
    , test "null type of existing field" <|
        \_ ->
            Selector.decodeString
                (Selector.field "status" [] (Selector.maybe Selector.int))
                json
                |> Expect.equal (Ok Nothing)
    , test "not existing field" <|
        \_ ->
            Selector.decodeString
                (Selector.field "height" [] (Selector.maybe Selector.int))
                json
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "{\n"
                        ++ "        \"name\": \"tom\",\n"
                        ++ "        \"age\": 42,\n"
                        ++ "        \"status\": null\n"
                        ++ "    }\n"
                        ++ "\n"
                        ++ "Expecting an OBJECT with a field named `height`"
                        |> Err
                    )
    , test "build graph" <|
        \_ ->
            Selector.render (Selector.maybe Selector.string)
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.maybe Selector.string)
                |> Selector.select "bar" [] (Selector.maybe Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
    , test "build field nested graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.maybe Selector.string)
                |> Selector.select "bar" [] (Selector.field "baz" [] (Selector.maybe Selector.string))
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
                |> Result.mapError Selector.errorToString
                |> Expect.equal (Err "Ran into a Json.Decode.oneOf with no possibilities!")
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
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "0\n"
                        ++ "\n"
                        ++ "Expecting a STRING"
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


oneOfMultipleNonFieldTests : List Test
oneOfMultipleNonFieldTests =
    let
        selector =
            Selector.oneOf
                [ Selector.map (User "user-id") Selector.string
                , Selector.map (Counter "counter-id") Selector.int
                ]
    in
    [ test "invalid source" <|
        \_ ->
            """
            {}
            """
                |> Selector.decodeString selector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Json.Decode.oneOf failed in the following 2 ways:\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "(1) Problem with the given value:\n"
                        ++ "    \n"
                        ++ "    {}\n"
                        ++ "    \n"
                        ++ "    Expecting a STRING\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "(2) Problem with the given value:\n"
                        ++ "    \n"
                        ++ "    {}\n"
                        ++ "    \n"
                        ++ "    Expecting an INT"
                        |> Err
                    )
    , test "valid `user` source" <|
        \_ ->
            """
            "Bob"
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok (User "user-id" "Bob"))
    , test "valid `counter` source" <|
        \_ ->
            """
            2
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok (Counter "counter-id" 2))
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
                [ Selector.field "username" [] Selector.string
                ]
    in
    [ test "invalid source" <|
        \_ ->
            """
            {}
            """
                |> Selector.decodeString selector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "{}\n"
                        ++ "\n"
                        ++ "Expecting an OBJECT with a field named `username`"
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


oneOfMultipleFieldTests : List Test
oneOfMultipleFieldTests =
    let
        selector =
            Selector.oneOf
                [ Selector.succeed User
                    |> Selector.select "id" [] Selector.string
                    |> Selector.select "username" [] Selector.string
                , Selector.succeed Counter
                    |> Selector.select "id" [] Selector.string
                    |> Selector.select "count" [] Selector.int
                ]
    in
    [ test "invalid source" <|
        \_ ->
            """
            {}
            """
                |> Selector.decodeString selector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Json.Decode.oneOf failed in the following 2 ways:\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "(1) Problem with the given value:\n"
                        ++ "    \n"
                        ++ "    {}\n"
                        ++ "    \n"
                        ++ "    Expecting an OBJECT with a field named `username`\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "(2) Problem with the given value:\n"
                        ++ "    \n"
                        ++ "    {}\n"
                        ++ "    \n"
                        ++ "    Expecting an OBJECT with a field named `count`"
                        |> Err
                    )
    , test "valid `user` source" <|
        \_ ->
            """
            {
                "id": "user-id",
                "username": "Bob"
            }
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok (User "user-id" "Bob"))
    , test "valid `counter` source" <|
        \_ ->
            """
            {
                "id": "counter-id",
                "count": 5
            }
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok (Counter "counter-id" 5))
    , test "graph" <|
        \_ ->
            Selector.render selector
                |> Expect.equal "id username id count"
    ]


oneOfNestedTests : List Test
oneOfNestedTests =
    let
        nestedSelector =
            Selector.succeed tuple2
                |> Selector.select "search" [] Selector.string
                |> Selector.select "results"
                    []
                    (Selector.list
                        (Selector.oneOf
                            [ Selector.succeed User
                                |> Selector.select "id" [] Selector.string
                                |> Selector.select "username" [] Selector.string
                            , Selector.succeed Counter
                                |> Selector.select "id" [] Selector.string
                                |> Selector.select "count" [] Selector.int
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
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "{}\n"
                        ++ "\n"
                        ++ "Expecting an OBJECT with a field named `results`"
                        |> Err
                    )
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
                        "id": "user-id-1",
                        "username": "Bob"
                    },
                    {
                        "id": "counter-id",
                        "count": 5
                    },
                    {
                        "id": "user-id-2",
                        "username": "Tom"
                    }
                ]
            }
            """
                |> Selector.decodeString nestedSelector
                |> Expect.equal
                    (Ok
                        ( "bar"
                        , [ User "user-id-1" "Bob"
                          , Counter "counter-id" 5
                          , User "user-id-2" "Tom"
                          ]
                        )
                    )
    , test "graph" <|
        \_ ->
            Selector.render nestedSelector
                |> Expect.equal "search results{id username id count}"
    ]


oneOfTests : List Test
oneOfTests =
    [ describe "GraphQL.Selector.oneOf with empty selector" oneOfEmptyTests
    , describe "GraphQL.Selector.oneOf with single non field selector" oneOfSingleNonFieldTests
    , describe "GraphQL.Selector.oneOf with multiple non field selector" oneOfMultipleNonFieldTests
    , describe "GraphQL.Selector.oneOf with single field selector" oneOfSingleFieldTests
    , describe "GraphQL.Selector.oneOf with multiple field selector" oneOfMultipleFieldTests
    , describe "GraphQL.Selector.oneOf with nested selector" oneOfNestedTests
    ]


mapTests : List Test
mapTests =
    let
        fieldSelector =
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.map String.length Selector.string)
                |> Selector.select "bar" [] (Selector.map ((+) 1) Selector.int)
    in
    [ test "Invalid source with direct selector" <|
        \_ ->
            """
            0
            """
                |> Selector.decodeString (Selector.map String.length Selector.string)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "0\n"
                        ++ "\n"
                        ++ "Expecting a STRING"
                        |> Err
                    )
    , test "valid source with direct selector" <|
        \_ ->
            """
            "string value"
            """
                |> Selector.decodeString (Selector.map String.length Selector.string)
                |> Expect.equal (Ok 12)
    , test "invalid source with field selector" <|
        \_ ->
            """
            {
                "foo": false,
                "bar": 1
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.foo:\n"
                        ++ "\n"
                        ++ "    false\n"
                        ++ "\n"
                        ++ "Expecting a STRING"
                        |> Err
                    )
    , test "valid source with field selector" <|
        \_ ->
            """
            {
                "foo": "string value",
                "bar": 4
            }
            """
                |> Selector.decodeString fieldSelector
                |> Expect.equal (Ok ( 12, 5 ))
    , test "build graph" <|
        \_ ->
            Selector.render (Selector.map String.length Selector.string)
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.map String.length Selector.string)
                |> Selector.select "bar" [] (Selector.map String.length Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
    , test "build field nested graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.map String.length Selector.string)
                |> Selector.select "bar" [] (Selector.field "baz" [] (Selector.map String.length Selector.string))
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
                        Selector.fail ("Expecting a positive number but instead got: " ++ Debug.toString x)

                    else
                        Selector.succeed x
                )

        fieldSelector =
            Selector.succeed tuple2
                |> Selector.select "bar"
                    []
                    (Selector.andThen
                        (\bar ->
                            case bar of
                                0 ->
                                    Selector.succeed tuple2
                                        |> Selector.select "first" [] Selector.bool
                                        |> Selector.select "second" [] Selector.string

                                1 ->
                                    Selector.succeed ( False, "empty" )

                                _ ->
                                    Selector.fail ("Invalid bar: " ++ String.fromInt bar)
                        )
                        Selector.int
                    )
                |> Selector.select "foo" [] (onlyPositive Selector.float)
    in
    [ test "Invalid source with direct selector" <|
        \_ ->
            """
            -1
            """
                |> Selector.decodeString (onlyPositive Selector.int)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "-1\n"
                        ++ "\n"
                        ++ "Expecting a positive number but instead got: -1"
                        |> Err
                    )
    , test "valid source with direct selector" <|
        \_ ->
            """
            1
            """
                |> Selector.decodeString (onlyPositive Selector.int)
                |> Expect.equal (Ok 1)
    , test "invalid source with field selector" <|
        \_ ->
            """
            {
                "foo": 3.14,
                "bar": false
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.bar:\n"
                        ++ "\n"
                        ++ "    false\n"
                        ++ "\n"
                        ++ "Expecting an INT"
                        |> Err
                    )
    , test "valid source and invalid andthen with field selector" <|
        \_ ->
            """
            {
                "foo": 3.14,
                "bar": 2
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.bar:\n"
                        ++ "\n"
                        ++ "    2\n"
                        ++ "\n"
                        ++ "Invalid bar: 2"
                        |> Err
                    )
    , test "valid source and valid hardcoded andthen with field selector" <|
        \_ ->
            """
            {
                "foo": 3.14,
                "bar": 1
            }
            """
                |> Selector.decodeString fieldSelector
                |> Expect.equal (Ok ( ( False, "empty" ), 3.14 ))
    , test "invalid source and valid andthen with field selector" <|
        \_ ->
            """
            {
                "foo": 3.14,
                "bar": 0
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.bar:\n"
                        ++ "\n"
                        ++ "    0\n"
                        ++ "\n"
                        ++ "Expecting an OBJECT with a field named `second`"
                        |> Err
                    )
    , test "build graph" <|
        \_ ->
            Selector.render (onlyPositive Selector.int)
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.render fieldSelector
                |> Expect.equal "bar foo"
    , test "build field nested graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "first" [] (Selector.map String.length Selector.string)
                |> Selector.select "second" [] fieldSelector
                |> Selector.render
                |> Expect.equal "first second{bar foo}"
    ]


succeedTests : List Test
succeedTests =
    let
        fieldSelector =
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.succeed 1)
                |> Selector.select "bar" [] (Selector.succeed True)
    in
    [ test "Direct selector" <|
        \_ ->
            """
            null
            """
                |> Selector.decodeString (Selector.succeed "str")
                |> Expect.equal (Ok "str")
    , test "field selector" <|
        \_ ->
            """
            {
                "foo": null,
                "bar": null
            }
            """
                |> Selector.decodeString fieldSelector
                |> Expect.equal (Ok ( 1, True ))
    , test "build graph" <|
        \_ ->
            Selector.render (Selector.succeed 3.14)
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.render fieldSelector
                |> Expect.equal "foo bar"
    , test "build field nested graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.succeed Nothing)
                |> Selector.select "bar" [] (Selector.field "baz" [] (Selector.succeed Nothing))
                |> Selector.render
                |> Expect.equal "foo bar{baz}"
    ]


failTests : List Test
failTests =
    let
        fieldSelector =
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.fail "message foo")
                |> Selector.select "bar" [] (Selector.fail "message bar")
    in
    [ test "Direct selector" <|
        \_ ->
            """
            null
            """
                |> Selector.decodeString (Selector.fail "message")
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "null\n"
                        ++ "\n"
                        ++ "message"
                        |> Err
                    )
    , test "field selector" <|
        \_ ->
            """
            {
                "foo": null,
                "bar": null
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.bar:\n"
                        ++ "\n"
                        ++ "    null\n"
                        ++ "\n"
                        ++ "message bar"
                        |> Err
                    )
    , test "build graph" <|
        \_ ->
            Selector.render (Selector.fail "message")
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.render fieldSelector
                |> Expect.equal "foo bar"
    , test "build field nested graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.fail "message foo")
                |> Selector.select "bar" [] (Selector.field "baz" [] (Selector.fail "message bar.baz"))
                |> Selector.render
                |> Expect.equal "foo bar{baz}"
    ]


nullTests : List Test
nullTests =
    let
        fieldSelector =
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.null False)
                |> Selector.select "bar" [] (Selector.null 1)
    in
    [ test "Same type source with direct selector" <|
        \_ ->
            """
            42
            """
                |> Selector.decodeString (Selector.null 42)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "42\n"
                        ++ "\n"
                        ++ "Expecting null"
                        |> Err
                    )
    , test "different type source with direct selector" <|
        \_ ->
            """
            false
            """
                |> Selector.decodeString (Selector.null 42)
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "false\n"
                        ++ "\n"
                        ++ "Expecting null"
                        |> Err
                    )
    , test "null source with direct selector" <|
        \_ ->
            """
            null
            """
                |> Selector.decodeString (Selector.null 42)
                |> Expect.equal (Ok 42)
    , test "same type source with field selector" <|
        \_ ->
            """
            {
                "foo": false,
                "bar": null
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.foo:\n"
                        ++ "\n"
                        ++ "    false\n"
                        ++ "\n"
                        ++ "Expecting null"
                        |> Err
                    )
    , test "different type source with field selector" <|
        \_ ->
            """
            {
                "foo": 0,
                "bar": null
            }
            """
                |> Selector.decodeString fieldSelector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the value at json.foo:\n"
                        ++ "\n"
                        ++ "    0\n"
                        ++ "\n"
                        ++ "Expecting null"
                        |> Err
                    )
    , test "valid source with field selector" <|
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
    , test "build graph" <|
        \_ ->
            Selector.render (Selector.keyValuePairs Selector.string)
                |> Expect.equal ""
    , test "build field graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.keyValuePairs Selector.string)
                |> Selector.select "bar" [] (Selector.keyValuePairs Selector.string)
                |> Selector.render
                |> Expect.equal "foo bar"
    , test "build field nested graph" <|
        \_ ->
            Selector.succeed tuple2
                |> Selector.select "foo" [] (Selector.keyValuePairs Selector.string)
                |> Selector.select "bar" [] (Selector.field "baz" [] (Selector.keyValuePairs Selector.string))
                |> Selector.render
                |> Expect.equal "foo bar{baz}"
    ]


type SearchResult
    = User String String
    | Counter String Int


onSingleEmptyTests : List Test
onSingleEmptyTests =
    let
        selector =
            Selector.on []
    in
    [ test "valid source" <|
        \_ ->
            """
            {}
            """
                |> Selector.decodeString selector
                |> Result.mapError Selector.errorToString
                |> Expect.equal (Err "Ran into a Json.Decode.oneOf with no possibilities!")
    , test "graph" <|
        \_ ->
            Selector.render selector
                |> Expect.equal ""
    ]


onSingleNonFieldTests : List Test
onSingleNonFieldTests =
    let
        selector =
            Selector.on
                [ ( "User", Selector.string )
                ]
    in
    [ test "valid source" <|
        \_ ->
            """
            {}
            """
                |> Selector.decodeString selector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "{}\n"
                        ++ "\n"
                        ++ "Expecting a STRING"
                        |> Err
                    )
    , test "graph" <|
        \_ ->
            Selector.render selector
                |> Expect.equal "...on User{}"
    ]


onMultipleNonFieldTests : List Test
onMultipleNonFieldTests =
    let
        selector =
            Selector.on
                [ ( "User", Selector.map (User "user-id") Selector.string )
                , ( "Counter", Selector.map (Counter "counter-id") Selector.int )
                ]
    in
    [ test "valid source" <|
        \_ ->
            """
            {}
            """
                |> Selector.decodeString selector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Json.Decode.oneOf failed in the following 2 ways:\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "(1) Problem with the given value:\n"
                        ++ "    \n"
                        ++ "    {}\n"
                        ++ "    \n"
                        ++ "    Expecting a STRING\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "(2) Problem with the given value:\n"
                        ++ "    \n"
                        ++ "    {}\n"
                        ++ "    \n"
                        ++ "    Expecting an INT"
                        |> Err
                    )
    , test "graph" <|
        \_ ->
            Selector.render selector
                |> Expect.equal "...on User{} ...on Counter{}"
    ]


onSingleFieldTests : List Test
onSingleFieldTests =
    let
        selector =
            Selector.on
                [ ( "User"
                  , Selector.field "username" [] Selector.string
                  )
                ]
    in
    [ test "invalid source" <|
        \_ ->
            """
            {}
            """
                |> Selector.decodeString selector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n"
                        ++ "{}\n"
                        ++ "\n"
                        ++ "Expecting an OBJECT with a field named `username`"
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
                |> Result.mapError Selector.errorToString
                |> Expect.equal (Ok "Bob")
    , test "graph" <|
        \_ ->
            Selector.render selector
                |> Expect.equal "...on User{username}"
    ]


onMultipeFieldTests : List Test
onMultipeFieldTests =
    let
        selector =
            Selector.on
                [ ( "User"
                  , Selector.succeed User
                        |> Selector.select "id" [] Selector.string
                        |> Selector.select "username" [] Selector.string
                  )
                , ( "Counter"
                  , Selector.succeed Counter
                        |> Selector.select "id" [] Selector.string
                        |> Selector.select "count" [] Selector.int
                  )
                ]
    in
    [ test "invalid source" <|
        \_ ->
            """
            {}
            """
                |> Selector.decodeString selector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Json.Decode.oneOf failed in the following 2 ways:\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "(1) Problem with the given value:\n"
                        ++ "    \n"
                        ++ "    {}\n"
                        ++ "    \n"
                        ++ "    Expecting an OBJECT with a field named `username`\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "\n"
                        ++ "(2) Problem with the given value:\n"
                        ++ "    \n"
                        ++ "    {}\n"
                        ++ "    \n"
                        ++ "    Expecting an OBJECT with a field named `count`"
                        |> Err
                    )
    , test "valid user source" <|
        \_ ->
            """
            {
                "id": "user-id",
                "username": "Bob"
            }
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok (User "user-id" "Bob"))
    , test "valid counter" <|
        \_ ->
            """
            {
                "id": "counter-id",
                "count": 5
            }
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok (Counter "counter-id" 5))
    , test "graph" <|
        \_ ->
            Selector.render selector
                |> Expect.equal "...on User{id username} ...on Counter{id count}"
    ]


onNestedTests : List Test
onNestedTests =
    let
        selector =
            Selector.succeed tuple2
                |> Selector.select "search" [] Selector.string
                |> Selector.select "results"
                    []
                    (Selector.list
                        (Selector.on
                            [ ( "User"
                              , Selector.succeed User
                                    |> Selector.select "id" [] Selector.string
                                    |> Selector.select "username" [] Selector.string
                              )
                            , ( "Counter"
                              , Selector.succeed Counter
                                    |> Selector.select "id" [] Selector.string
                                    |> Selector.select "count" [] Selector.int
                              )
                            ]
                        )
                    )
    in
    [ test "invalid source" <|
        \_ ->
            """
            {}
            """
                |> Selector.decodeString selector
                |> Result.mapError Selector.errorToString
                |> Expect.equal
                    ("Problem with the given value:\n"
                        ++ "\n{}\n"
                        ++ "\nExpecting an OBJECT with a field named `results`"
                        |> Err
                    )
    , test "valid empty source" <|
        \_ ->
            """
            {
                "search": "foo",
                "results": []
            }
            """
                |> Selector.decodeString selector
                |> Expect.equal (Ok ( "foo", [] ))
    , test "valid mixed source" <|
        \_ ->
            """
            {
                "search": "bar",
                "results": [
                    {
                        "id": "user-id-1",
                        "username": "Bob"
                    },
                    {
                        "id": "counter-id",
                        "count": 5
                    },
                    {
                        "id": "user-id-2",
                        "username": "Tom"
                    }
                ]
            }
            """
                |> Selector.decodeString selector
                |> Expect.equal
                    (Ok
                        ( "bar"
                        , [ User "user-id-1" "Bob"
                          , Counter "counter-id" 5
                          , User "user-id-2" "Tom"
                          ]
                        )
                    )
    , test "graph" <|
        \_ ->
            Selector.render selector
                |> Expect.equal "search results{...on User{id username} ...on Counter{id count}}"
    ]


onTests : List Test
onTests =
    [ describe "GraphQL.Selector.on with single empty selector" onSingleEmptyTests
    , describe "GraphQL.Selector.on with single non field selector" onSingleNonFieldTests
    , describe "GraphQL.Selector.on with multiple non field selector" onMultipleNonFieldTests
    , describe "GraphQL.Selector.on with single field selector" onSingleFieldTests
    , describe "GraphQL.Selector.on with multiple field selector" onMultipeFieldTests
    , describe "GraphQL.Selector.on with nested selector" onNestedTests
    ]
