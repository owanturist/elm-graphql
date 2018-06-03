module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (Test, describe, test)


foo : Test
foo =
    describe "foo"
        [ test "hi"
            (\_ ->
                Expect.equal 0 0
            )
        ]
