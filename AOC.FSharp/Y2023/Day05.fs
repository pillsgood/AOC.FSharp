namespace AOC.FSharp.Y2023

open System.Text.RegularExpressions
open AOC.FSharp.Common
open NUnit.Framework
open Pillsgood.AdventOfCode

type Day05() =
    inherit AocFixture()
    let input = base.Input.Get<string[]>()

    let seeds =
        input[0]
        |> function
            | Matches Regex(@"\d+")

    let cards =

        let pattern = Regex(@"Card\s*(\d+): ((?:\s*\d+)+) \| ((?:\s*\d+)+)")
        let map f (x, y) = (f x), (f y)

        let parse =
            function
            | Match pattern [ id; pot; jackpot ] ->
                Some
                    { id = int id
                      count = (pot, jackpot) |> map (String.split " ") ||> Seq.intersect |> Seq.length }
            | _ -> None

        base.Input.Get<string[]>() |> Seq.choose parse |> Seq.toList

    [<Test>]
    member _.Part1() = ignore

    [<Test>]
    member _.Part2() = ignore
