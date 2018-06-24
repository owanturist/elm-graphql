module GraphQL exposing (GraphQL)

import GraphQL.Argument as Argument exposing (Argument)
import GraphQL.Selector as Selector exposing (Selector)


type GraphQL a
    = GraphQL String (Maybe String) (Selector a)


query : Selector a -> GraphQL a
query =
    GraphQL "query" Nothing


mutation : Selector a -> GraphQL a
mutation =
    GraphQL "mutation" Nothing


subscription : Selector a -> GraphQL a
subscription =
    GraphQL "subscription" Nothing


named : String -> GraphQL a -> GraphQL a
named name (GraphQL operation _ decoder) =
    GraphQL operation (Just name) decoder


type alias Bar =
    { foo : Foo
    , bar : List Foo
    }


type alias Foo =
    { bar : String
    , baz : Int
    }


foo : Selector Foo
foo =
    Selector.field "baz" [] Selector.int (Selector.field "foo" [] Selector.string (Selector.succeed Foo))


bar : Selector Bar
bar =
    Selector.succeed Bar
        |> Selector.field "foo"
            [ ( "id", Argument.int 20 )
            , ( "sort", Argument.string "-" )
            ]
            foo
        |> Selector.field "baz" [] (Selector.list foo)
