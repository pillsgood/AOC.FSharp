namespace AOC.FSharp.Y2023

open System.Text.RegularExpressions
open NUnit.Framework
open Pillsgood.AdventOfCode
open AOC.FSharp.Common

[<AocFixture>]
module Day08 =
    type Direction =
        | Left
        | Right

    type Node =
        { Id: string
          Left: string
          Right: string }

        member this.Item
            with get dir =
                match dir with
                | Left -> this.Left
                | Right -> this.Right

    let input = Input.fetch<string[]>

    let instructions =
        input[0]
        |> Seq.choose (function
            | 'R' -> Some Right
            | 'L' -> Some Left
            | _ -> None)
        |> Seq.toList

    let network =
        let parse =
            function
            | MatchValue (Regex(@"(?<id>\w+) = \((?<left>\w+), (?<right>\w+)\)")) [ id; left; right ] ->
                Some
                    { Id = id
                      Left = left
                      Right = right }
            | _ -> None

        input[1..] |> Seq.choose parse |> Seq.addKey _.Id

    let traverse c start =
        Seq.initInfinite (fun i -> instructions[i % instructions.Length])
        |> Seq.scan (fun n dir -> network[n[dir]]) start
        |> Seq.takeWhile c
        |> Seq.length

    [<Test>]
    let Part1 () = network["AAA"] |> traverse (fun node -> node.Id <> "ZZZ") |> Answer.submit

    [<Test>]
    let Part2 () =
        let start = network |> Map.values |> Seq.filter _.Id.EndsWith("A")

        let lcm xs =
            let rec gcd a b =
                let remainder = a % b
                if remainder = 0L then b else gcd b remainder

            xs |> Seq.reduce (fun a b -> a * b / (gcd a b))

        start
        |> Seq.map (traverse (fun n -> not (n.Id.EndsWith "Z")))
        |> Seq.map int64
        |> lcm
        |> Answer.submit
