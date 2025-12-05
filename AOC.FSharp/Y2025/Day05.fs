namespace AOC.FSharp.Y2025

open System.Text.RegularExpressions
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day05 =
    let input =
        Input.fetch<string array>
        |> Array.choose (function
            | MatchValue (Regex @"^(\d+)-(\d+)$") [ s; e ] -> Some(Some(int64 s, int64 e), None)
            | MatchValue (Regex @"^(\d+)$") [ n ] -> Some(None, Some(int64 n))
            | _ -> None)
        |> fun src -> {| Ranges = src |> Array.choose fst; Products = src |> Array.choose snd |}

    [<Test>]
    let Part1 () =
        let isFresh =
            input.Ranges
            |> Array.map (fun (s, e) n -> n >= s && n <= e)
            |> Array.reduce (fun f g -> fun n -> f n || g n)

        input.Products |> Seq.count isFresh |> Answer.submit

    [<Test>]
    let Part2 () =
        input.Ranges
        |> Array.sortBy fst
        |> Array.fold
            (fun acc (s', e') ->
                match acc with
                | [] -> [ (s', e') ]
                | (s, e) :: rem when s' <= e + 1L -> (s, max e e') :: rem
                | _ -> (s', e') :: acc)
            []
        |> List.sumBy (fun (s, e) -> e - s + 1L)
        |> Answer.submit
