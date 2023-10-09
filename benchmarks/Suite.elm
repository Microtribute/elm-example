module Suite exposing (suite)

import Benchmark exposing (..)
import MedalStanding
    exposing
        ( populateMedalStandings
        , populateMedalStandings2
        , populateMedalStandings3
        , populateMedalStandings4
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
    in
    describe "Medal Standings"
        [ -- nest as many descriptions as you like
          describe "Sorting medal standings"
            [ benchmark "method 1 (default)" method1
            , benchmark "method 2 (barebone)" method2
            , benchmark "method 3 (stringified)" method3
            , benchmark "method 4 (listing)" method4
            ]
        ]
