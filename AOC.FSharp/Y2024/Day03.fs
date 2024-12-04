namespace AOC.FSharp.Y2024

open System.Text.RegularExpressions
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

    [<Test>]
    let Part2 () =

        let pattern = Regex(@"don't\(\).*?(do\(\))|don't\(\).*", options = RegexOptions.Singleline)
        let input = (input, "") |> pattern.Replace

        input
        |> Regex.matches @"mul\((\d+)\,(\d+)\)"
        |> Seq.map (fun m -> int m.Groups[1].Value, int m.Groups[2].Value)
        |> Seq.sumBy (fun (i, j) -> i * j)
        |> Answer.submit
