module Suite exposing (suite)

import Benchmark exposing (..)
import MedalStanding
    exposing
        ( populateMedalStandings
        , populateMedalStandings2
        , populateMedalStandings3
        , populateMedalStandings4
        , populateMedalStandings5
        , populateMedalStandings6
        )


suite : Benchmark
suite =
    let
        method1 =
            \() -> populateMedalStandings 100

        method2 =
            \() -> populateMedalStandings2 100

        method3 =
            \() -> populateMedalStandings3 100

        method4 =
            \() -> populateMedalStandings4 100

        method5 =
            \() -> populateMedalStandings5 100

        method6 =
            \() -> populateMedalStandings6 100
    in
    describe "Medal Standings"
        [ -- nest as many descriptions as you like
          describe "Sorting medal standings"
            [ benchmark "populateMedalStandings" method1
            , benchmark "populateMedalStandings2" method2
            , benchmark "populateMedalStandings3" method3
            , benchmark "populateMedalStandings4" method4
            , benchmark "populateMedalStandings5" method5
            , benchmark "populateMedalStandings6" method6
            ]
        ]
