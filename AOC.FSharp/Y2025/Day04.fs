namespace AOC.FSharp.Y2025

open AOC.FSharp.Common
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day04 =
    let input =
        Input.fetch<string array>
        |> Array.choose2d (fun i j c ->
            match c with
            | '@' -> Some(int2 (i, j))
            | _ -> None)
        |> Set.ofArray

    let canRemove (state: int2 Set) position =
        let countNeighbors (position: int2) =
            Seq.append int2.cardinalDirections int2.ordinalDirections
            |> Seq.map ((+) position)
            |> Seq.count state.Contains

        countNeighbors position < 4

    [<Test>]
    let Part1 () = input |> Seq.count (canRemove input) |> Answer.submit

    [<Test>]
    let Part2 () =
        input
        |> Seq.unfold (fun state ->
            state
            |> Set.filter (canRemove state)
            |> (fun s -> if s.IsEmpty then None else Some(s.Count, state - s)))
        |> Seq.sum
        |> Answer.submit
