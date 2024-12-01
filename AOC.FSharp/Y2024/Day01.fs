namespace AOC.FSharp.Y2024

open System.Text.RegularExpressions
open Microsoft.FSharp.Collections
open NUnit.Framework
open Pillsgood.AdventOfCode
open AOC.FSharp.Common

[<AocFixture>]
module Day01 =

    let input: string array = Input.fetch

    let locations: (int * int) array =
        input
        |> Seq.choose (function
            | MatchValue (Regex "(\d+)\s*(\d+)") [ left; right ] -> Some(int left, int right)
            | _ -> None)
        |> Seq.toArray

    [<Test>]
    let Part1 () =
        let left = locations |> Seq.map fst |> Seq.sort
        let right = locations |> Seq.map snd |> Seq.sort

        Seq.zip left right |> Seq.sumBy (fun (l, r) -> abs (l - r)) |> Answer.submit


    [<Test>]
    let Part2 () =
        let right = locations |> Seq.countBy snd |> Map

        locations
        |> Seq.map fst
        |> Seq.sumBy (fun value -> value * (right |> Map.tryFind value |> Option.defaultValue 0))
        |> Answer.submit
