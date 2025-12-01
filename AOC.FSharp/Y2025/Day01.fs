namespace AOC.FSharp.Y2025

open System.Text.RegularExpressions
open Microsoft.FSharp.Collections
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day01 =

    let input =
        Input.fetch
        |> String.splitLines
        |> Array.map (function
            | MatchValue (Regex "L(\d+)") [ value ] -> -(int value)
            | MatchValue (Regex "R(\d+)") [ value ] -> (int value)
            | _ -> failwith "Invalid input")

    let (%) = Math.modulo

    [<Test>]
    let Part1 () =
        (50, input)
        ||> Seq.scan (fun acc dial -> (acc + dial) % 100)
        |> Seq.count ((=) 0)
        |> Answer.submit

    [<Test>]
    let Part2 () =
        input
        |> Seq.collect (fun i -> Seq.replicate (abs i) (sign i))
        |> Seq.scan (fun acc dial -> (acc + dial) % 100) 50
        |> Seq.count ((=) 0)
        |> Answer.submit
