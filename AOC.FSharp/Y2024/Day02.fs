namespace AOC.FSharp.Y2024

open AOC.FSharp.Common
open Microsoft.FSharp.Collections
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day03 =

    let input: string = Input.fetch

    [<Test>]
    let Part1 () =
        input
        |> Regex.matches @"mul\((\d+)\,(\d+)\)"
        |> Seq.map (fun m -> int m.Groups[1].Value, int m.Groups[2].Value)
        |> Seq.sumBy (fun (i, j) -> i * j)
        |> Answer.submit

    type Instruction =
        | Mul of i: int * j: int
        | Take
        | Skip

    [<Test>]
    let Part2 () =

        let muls =
            input
            |> Regex.matches "mul\((\d+)\,(\d+)\)"
            |> Seq.map (fun m -> m.Index, Mul(int m.Groups[1].Value, int m.Groups[2].Value))

        let takes = input |> Regex.matches "do\(\)" |> Seq.map (fun m -> m.Index, Take)
        let skips = input |> Regex.matches "don\'t\(\)" |> Seq.map (fun m -> m.Index, Skip)

        Seq.concat [ muls; takes; skips ]
        |> Seq.sortBy fst
        |> Seq.map snd
        |> Seq.splitBy (fun step -> step.IsSkip || step.IsTake)
        |> Seq.collect (fun steps -> if (steps |> Array.head).IsSkip then Seq.empty else steps)
        |> Seq.sumBy (function
            | Mul(i, j) -> i * j
            | _ -> 0)
        |> Answer.submit
