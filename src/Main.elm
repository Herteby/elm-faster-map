module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (..)


recurse : (a -> b) -> List b -> List a -> List b
recurse fn output list =
    case list of
        x :: xs ->
            recurse fn (fn x :: output) xs

        [] ->
            output


reverse : List a -> List a
reverse list =
    recurse identity [] list


map : (a -> b) -> List a -> List b
map fn list =
    recurse fn [] list |> reverse


l10 =
    List.range 0 10


l100 =
    List.range 0 100


l1000 =
    List.range 0 1000


l10000 =
    List.range 0 10000


suite : Benchmark
suite =
    Benchmark.describe ""
        [ Benchmark.compare "10 iterations"
            "List.map"
            (\_ ->
                List.map ((+) 1) l10
            )
            "Tail call map"
            (\_ ->
                map ((+) 1) l10
            )
        , Benchmark.compare "100 iterations"
            "List.map"
            (\_ ->
                List.map ((+) 1) l100
            )
            "Tail call map"
            (\_ ->
                map ((+) 1) l100
            )
        , Benchmark.compare "1,000 iterations"
            "List.map"
            (\_ ->
                List.map ((+) 1) l1000
            )
            "Tail call map"
            (\_ ->
                map ((+) 1) l1000
            )
        , Benchmark.compare "10,000 iterations"
            "List.map"
            (\_ ->
                List.map ((+) 1) l10000
            )
            "Tail call map"
            (\_ ->
                map ((+) 1) l10000
            )

        --reverse
        , Benchmark.compare "10 iterations"
            "List.reverse"
            (\_ ->
                List.reverse l10
            )
            "New reverse"
            (\_ ->
                reverse l10
            )
        , Benchmark.compare "100 iterations"
            "List.reverse"
            (\_ ->
                List.reverse l100
            )
            "New reverse"
            (\_ ->
                reverse l100
            )
        , Benchmark.compare "1,000 iterations"
            "List.reverse"
            (\_ ->
                List.reverse l1000
            )
            "New reverse"
            (\_ ->
                reverse l1000
            )
        , Benchmark.compare "10,000 iterations"
            "List.reverse"
            (\_ ->
                List.reverse l10000
            )
            "New reverse"
            (\_ ->
                reverse l10000
            )
        ]


main : BenchmarkProgram
main =
    program suite
