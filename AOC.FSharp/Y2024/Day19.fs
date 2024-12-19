namespace AOC.FSharp.Y2024

open System.Text.RegularExpressions
open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day19 =
    let patterns, designs =
        Input.fetch<string>
        |> String.split "\n\n"
        |> (fun l ->
            (l[0] |> String.split ",") |> Array.sortByDescending (fun s -> s.Length, s), (l[1] |> String.splitLines))

    let unitPatterns =
        patterns
        |> Array.indexed
        |> Array.filter (fun (i, p) ->
            p.Length = 1
            || not (Regex.IsMatch(p, (patterns[(i + 1) ..] |> String.concat "|" |> (fun s -> $"^({s})+$")))))
        |> Array.map snd

    [<Test>]
    let Part1 () =
        let pattern = unitPatterns |> (String.concat "|" >> (fun s -> Regex($"^({s})+$")))
        designs |> Seq.count pattern.IsMatch |> Answer.submit

    [<Test>]
    let Part2 () =
        let patterns = List.ofArray patterns

        let countAllCombinations =
            memoizeRec (fun f' (case: string) ->
                patterns
                |> List.indexed
                |> List.filter (snd >> case.StartsWith)
                |> List.sumBy (fun (_, p) ->
                    let remaining = case[p.Length ..]
                    if remaining.Length = 0 then 1L else f' remaining))

        unitPatterns
        |> (String.concat "|" >> (fun s -> Regex($"^({s})+$")))
        |> fun pattern -> designs |> Array.filter pattern.IsMatch
        |> Array.Parallel.sumBy countAllCombinations
        |> Answer.submit
