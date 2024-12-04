namespace AOC.FSharp.Y2024

open System
open AOC.FSharp.Common
open Microsoft.FSharp.Collections
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day04 =
    let input: Map<int2, char> =
        Input.fetch<string array>
        |> Array.mapi (fun j str -> str |> Seq.mapi (fun i c -> int2 (i, j), c))
        |> Seq.concat
        |> Map

    [<Test>]
    let Part1 () =
        let scan position =
            Seq.append int2.cardinalDirections int2.ordinalDirections
            |> Seq.map (fun dir ->
                Array.init 4 (fun i -> position + (dir * i))
                |> (Array.choose input.TryFind >> String))
            |> Seq.count (fun str -> str = "XMAS")

        input
        |> Map.filter (fun _ c -> c = 'X')
        |> Seq.sumBy (_.Key >> scan)
        |> Answer.submit

    [<Test>]
    let Part2 () =
        let expand start position =
            let start = start + int2.up
            [| position + start; position; position - start |]

        let scan position =
            [ position |> expand int2.left; position |> expand int2.right ]
            |> Seq.map (Array.choose input.TryFind >> String)
            |> Seq.forall (fun str -> str = "MAS" || str = "SAM")

        input
        |> Map.filter (fun _ c -> c = 'A')
        |> Seq.count (_.Key >> scan)
        |> Answer.submit
