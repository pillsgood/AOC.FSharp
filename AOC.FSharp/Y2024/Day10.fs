namespace AOC.FSharp.Y2024

open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day10 =
    let input: Map<int2, int2 * int> =
        Input.fetch<string array>
        |> Array.rev
        |> Array.mapi (fun j str -> str |> Seq.mapi (fun i c -> int2 (i, j), int (c - '0')))
        |> Seq.concat
        |> Seq.map (fun t -> fst t, t)
        |> Map

    let heads = input.Values |> Seq.filter (fun (_, v) -> v = 0) |> Seq.toArray

    let rec pathFind (position, height) =
        let inline search (position, height) =
            int2.cardinalDirections
            |> Array.choose (fun v -> input |> Map.tryFind (v + position))
            |> Array.filter (snd >> (=) (height + 1))
            |> Array.collect pathFind

        (position, height) |> if height = 9 then Array.singleton else search

    [<Test>]
    let Part1 () =
        heads
        |> Array.Parallel.collect (pathFind >> Array.distinct)
        |> Array.length
        |> Answer.submit

    [<Test>]
    let Part2 () = heads |> Array.Parallel.collect pathFind |> Array.length |> Answer.submit
