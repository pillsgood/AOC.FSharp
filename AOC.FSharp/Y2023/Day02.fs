namespace AOC.FSharp.Y2023

open NUnit.Framework
open FSharp.Text.RegexProvider
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day02 =
    type Color =
        | Red
        | Green
        | Blue

    type CubeSet = { Count: int; Color: Color }
    type Game = { GameId: int; Sets: CubeSet list }

    type SetPattern = Regex< @"(?<count>\d+) (?<color>red|green|blue)" >
    type GamePattern = Regex< @"Game (?<gameId>\d+):" >

    let mutable input: Game[] =
        let color str =
            match str with
            | "red" -> Red
            | "green" -> Green
            | "blue" -> Blue
            | _ -> failwith "out of pattern"

        let cubeSets str =
            SetPattern().TypedMatches str
            |> Seq.map (fun m -> { Count = int m.count.Value; Color = color m.color.Value })

        let parse line : Game =
            let capture = GamePattern().TypedMatch line
            let sets = cubeSets line |> Seq.toList

            { GameId = int capture.gameId.Value; Sets = sets }

        Input.fetch |> Array.map parse

    [<Test>]
    let Part1 () =
        let isLegalSet (set: CubeSet) =
            match set with
            | { Color = Red } -> set.Count <= 12
            | { Color = Green } -> set.Count <= 13
            | { Color = Blue } -> set.Count <= 14

        input
        |> Seq.where (fun game -> game.Sets |> Seq.forall isLegalSet)
        |> Seq.sumBy _.GameId
        |> Answer.submit

    [<Test>]
    let Part2 () =
        let power (game: Game) =
            let getMax f = Seq.maxBy f >> f

            game.Sets
            |> Seq.groupBy _.Color
            |> Seq.map (fun (_, sets) -> sets |> getMax _.Count)
            |> Seq.fold (*) 1

        input |> Seq.sumBy power |> Answer.submit
