port module Main exposing (..)

import Exercise01.Tests
import Exercise02.Tests
import Test exposing (Test, describe)
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


allTests : Test
allTests =
    describe "All exercises"
        [ Exercise01.Tests.all
        , Exercise02.Tests.all
        ]


main : TestProgram
main =
    run emit Exercise01.Tests.all


port emit : ( String, Value ) -> Cmd msg