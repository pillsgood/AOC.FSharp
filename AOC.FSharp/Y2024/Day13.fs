namespace AOC.FSharp.Y2024

open System.Diagnostics.CodeAnalysis
open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day13 =
    [<Struct>]
    type Machine =
        { coeffA: long2
          coeffB: long2
          target: long2 }

    module Patterns =
        [<Literal; StringSyntax("regex")>]
        let buttonA = @"Button A: X\+(?<ax>\d+), Y\+(?<ay>\d+)"

        [<Literal; StringSyntax("regex")>]
        let buttonB = @"Button B: X\+(?<bx>\d+), Y\+(?<by>\d+)"

        [<Literal; StringSyntax("regex")>]
        let prize = @"Prize: X=(?<tx>\d+), Y=(?<ty>\d+)"

    let input: Machine[] =
        Input.fetch<string>
        |> Regex.matches (
            [| Patterns.buttonA; Patterns.buttonB; Patterns.prize |]
            |> String.concat @"\r?\n"
        )
        |> Array.map (fun m ->
            { coeffA = long2 (int m.Groups["ax"].Value, int m.Groups["ay"].Value)
              coeffB = long2 (int m.Groups["bx"].Value, int m.Groups["by"].Value)
              target = long2 (int m.Groups["tx"].Value, int m.Groups["ty"].Value) })

    let findMinCost (machine: Machine) =
        let (<*>) (v1: long2) (v2: long2) = (v1.x * v2.y - v1.y * v2.x)
        let d = (machine.coeffA <*> machine.coeffB)
        let a = (machine.target <*> machine.coeffB) / +d
        let b = (machine.target <*> machine.coeffA) / -d

        match (machine.coeffA * a) + (machine.coeffB * b) = machine.target with
        | true -> Some(3L * a + b)
        | _ -> None

    [<Test>]
    let Part1 () = input |> Seq.choose findMinCost |> Seq.sum |> Answer.submit

    [<Test>]
    let Part2 () =
        input
        |> Array.map (fun m -> { m with target = m.target + long2.one * 10000000000000L })
        |> Array.choose findMinCost
        |> Array.sum
        |> Answer.submit
