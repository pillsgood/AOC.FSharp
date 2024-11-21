namespace AOC.FSharp.Y2023

open System.Text.RegularExpressions
open NUnit.Framework
open Pillsgood.AdventOfCode
open AOC.FSharp.Common

[<AocFixture>]
module Day09 =
    let input: string[] = Input.fetch

    let sequences =
        input
        |> Seq.choose (function
            | Matches (Regex(@"-?\d+")) groups -> Some(groups |> List.map (_.Value >> int64))
            | _ -> None)
        |> Seq.toList

    let rec extrapolate xs =
        let rec scan xs acc =
            let ds = xs |> Seq.pairwise |> Seq.map (fun (a, b) -> b - a) |> Seq.toList
            let i = acc + List.last xs

            match ds |> Seq.sum with
            | 0L -> i
            | _ -> scan ds i

        scan xs 0L

    [<Test>]
    let Part1 () = sequences |> Seq.sumBy extrapolate |> Answer.submit

    [<Test>]
    let Part2 () = sequences |> List.map List.rev |> Seq.sumBy extrapolate |> Answer.submit
