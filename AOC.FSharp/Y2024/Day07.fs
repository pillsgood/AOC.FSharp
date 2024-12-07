namespace AOC.FSharp.Y2024

open System.Text.RegularExpressions
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day07 =
    let input =
        Input.fetch<string array>
        |> Array.choose (function
            | Match (Regex @"(\d+)\:(?:\s?(\d+))+") group ->
                Some(int64 group[0].Value, group[1].Captures |> Seq.map (_.Value >> int64) |> Seq.toArray)
            | _ -> None)

    type op = int64 -> int64 -> int64

    let testCalibration (operators: op list) (expected: int64, values: int64 array) =
        let baseLength = operators.Length
        let possibilities = pown baseLength (values.Length - 1)

        Seq.init possibilities (fun x ->
            [ for i in 0 .. values.Length - 2 -> operators[(x / (pown baseLength i)) % baseLength] ])
        |> Seq.exists (fun ops -> values |> Seq.reducei (fun a b i -> (ops |> List.item i) a b) = expected)

    [<Test>]
    let Part1 () =
        input
        |> Array.Parallel.filter (testCalibration [ (+); (*) ])
        |> Array.sumBy fst
        |> Answer.submit

    [<Test>]
    let Part2 () =
        let concat a b = a * pown 10L (max 1 (int (log10 (float (max 1L b))) + 1)) + b

        input
        |> Array.Parallel.filter (testCalibration [ (+); (*); concat ])
        |> Array.sumBy fst
        |> Answer.submit
