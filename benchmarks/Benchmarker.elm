module Benchmarker exposing (..)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Suite exposing (suite)


main : BenchmarkProgram
main =
    program suite
