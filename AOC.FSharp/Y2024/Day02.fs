namespace AOC.FSharp.Y2024

open Microsoft.FSharp.Collections
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day02 =

    let input: int array array = Input.fetch |> Array.map (String.split " " >> (Array.map int))

    let validate (level: int seq) : bool =
        level
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> a - b)
        |> Seq.cache
        |> fun level ->
            let sign = sign (Seq.head level)
            level |> Seq.forall (fun x -> (x * sign) |> fun x -> x >= 1 && x <= 3)

    [<Test>]
    let Part1 () = input |> Seq.filter validate |> Seq.length |> Answer.submit

    [<Test>]
    let Part2 () =
        let testLevel (level: int array) =
            Seq.init level.Length (fun i -> level |> Seq.removeAt i) |> Seq.exists validate

        input |> Seq.filter testLevel |> Seq.length |> Answer.submit
