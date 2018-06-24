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


selectorSheet : Test
selectorSheet =
    describe "Test GraphQL.Selector builder functions"
        [ test "Empty graph" <|
            \_ ->
                Selector.string
                    |> Selector.select
                    |> Tuple.first
                    |> Expect.equal Nothing
        , test "Single graph" <|
            \_ ->
                Selector.succeed identity
                    |> Selector.field "bar" [] Selector.string
                    |> Selector.select
                    |> Tuple.first
                    |> Expect.equal (Just "bar")
        , test "Multiple graph" <|
            \_ ->
                Selector.succeed (,,)
                    |> Selector.field "bar" [] Selector.string
                    |> Selector.field "foo" [] Selector.string
                    |> Selector.field "baz" [] Selector.string
                    |> Selector.select
                    |> Tuple.first
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
                    |> Selector.select
                    |> Tuple.first
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
                    |> Selector.select
                    |> Tuple.first
                    |> Expect.equal (Just "bar foo{bar1 foo1{bar2 foo2 baz2} baz1} baz")
        , test "Argumented graph" <|
            \_ ->
                Selector.succeed identity
                    |> Selector.field "bar"
                        [ ( "foo", Argument.string "baz" )
                        ]
                        Selector.string
                    |> Selector.select
                    |> Tuple.first
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
                    |> Selector.select
                    |> Tuple.first
                    |> Expect.equal (Just "bar(foo:\"baz\"){bar1(foo1:\"baz1\")}")
        , test "Aliased graph" <|
            \_ ->
                Selector.succeed identity
                    |> Selector.aliased "foo" "bar" [] Selector.string
                    |> Selector.select
                    |> Tuple.first
                    |> Expect.equal (Just "foo:bar")
        , test "Aliased argumented graph" <|
            \_ ->
                Selector.succeed identity
                    |> Selector.aliased "foo"
                        "bar"
                        [ ( "baz", Argument.int 0 )
                        ]
                        Selector.string
                    |> Selector.select
                    |> Tuple.first
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
                    |> Selector.select
                    |> Tuple.first
                    |> Expect.equal (Just "bar:bar_zero(str:\"zero\",int:0) foo:foo_zero(str:\"zero\",int:0){bar1:bar_first(str:\"first\",int:1) foo1:foo_first(str:\"first\",int:1){bar2:bar_second(str:\"second\",int:2) foo2:foo_second(str:\"second\",int:2) baz2:baz_second(str:\"second\",int:2)} baz1:baz_first(str:\"first\",int:1)} baz:baz_zero(str:\"zero\",int:0)")
        ]
