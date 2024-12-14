namespace AOC.FSharp.Y2024

open System.Text.RegularExpressions
open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day14 =
    [<Struct>]
    type Bot = { position: int2; velocity: int2 }

    let bounds = rectInt (int2.zero, int2 (101, 103))

    let input: Bot array =
        let pattern = Regex @"p=(?<px>\d+)\,(?<py>\d+) v=(?<vx>-?\d+),(?<vy>-?\d+)"

        Input.fetch<string array>
        |> Array.choose (function
            | MatchValue pattern [ px; py; vx; vy ] ->
                Some({ position = int2 (int px, int py); velocity = int2 (int vx, int vy) })
            | _ -> None)

    let move duration (bot: Bot) =
        let (%) = Math.modulo
        let p = bot.position + (bot.velocity * duration)
        int2 (p.x % bounds.width, p.y % bounds.height)

    let quadrants =
        let split op x = x / 2 |> fun x -> op x (x % 2)
        let bounds = bounds.size
        let size = int2 (split (-) bounds.x, split (-) bounds.y)

        [ rectInt (int2.zero, size)
          rectInt (int2 (split (+) bounds.x, 0), size)
          rectInt (int2 (0, split (+) bounds.y), size)
          rectInt (int2 (split (+) bounds.x, split (+) bounds.y), size) ]

    [<Test>]
    let Part1 () =
        input
        |> Array.map (move 100)
        |> Seq.countBy (fun p -> quadrants |> Seq.tryFind _.Contains(p))
        |> Seq.choose (fun (q, c) -> q |> Option.map (konst c))
        |> Seq.reduce (*)
        |> Answer.submit

    [<Test>]
    let Part2 () =
        Seq.initInfinite (fun i -> input |> Array.map (move i) |> Set)
        |> Seq.indexed
        |> Seq.find (fun (_, bots) -> bots.Count = input.Length)
        |> (fst >> Answer.submit)
