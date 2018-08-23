module Tests exposing (tests)

import Test exposing (Test, describe)
import Tests.Argument
import Tests.Selector


tests : Test
tests =
    describe "GraphQL"
        [ Tests.Argument.tests
        , Tests.Selector.tests
        ]
