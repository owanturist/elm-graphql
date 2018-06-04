module Tests exposing (..)

import Array
import Expect exposing (Expectation)
import Fuzz
import GraphQL.Argument as Argument
import GraphQL.Internal as Internal
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
