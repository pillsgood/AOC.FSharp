namespace AOC.FSharp.Y2024

open Microsoft.FSharp.Collections
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day02 =

    let input: int array array = Input.fetch |> Array.map (String.split " " >> (Array.map int))

    let validate (level: int seq) : bool =
        let inline exclusive min max value = value >= min && value <= max

        let isMonotonic = level |> Seq.pairwise |> Seq.allEqualBy (fun (i, j) -> sign (i - j))
        let isGrowthInRange = level |> Seq.pairwise |> Seq.forall (fun (i, j) -> abs (i - j) |> exclusive 1 3)
        isMonotonic && isGrowthInRange

    [<Test>]
    let Part1 () = input |> Seq.filter validate |> Seq.length |> Answer.submit

    [<Test>]
    let Part2 () =
        let testLevel (level: int array) = Seq.init level.Length (fun i -> level |> Seq.removeAt i) |> Seq.exists validate

        input |> Seq.filter testLevel |> Seq.length |> Answer.submit
