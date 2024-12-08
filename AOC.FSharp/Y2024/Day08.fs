namespace AOC.FSharp.Y2024

open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day08 =
    let input: string array = Input.fetch

    let bounds = rectInt (int2.zero, (Array.dimensions input) - int2.one)

    let map =
        input
        |> Seq.mapi (fun j str -> str |> Seq.mapi (fun i c -> int2 (i, bounds.height - j), c))
        |> Seq.concat
        |> Seq.filter (snd >> (<>) '.')
        |> Map

    let nodes =
        map
        |> Seq.groupBy _.Value
        |> Seq.map (fun (signal, values) -> signal, values |> Seq.map _.Key |> Seq.toArray)

    [<Test>]
    let Part1 () =
        let antiNodes (a, b) = [ a + (a - b); b + (b - a) ]

        nodes
        |> Seq.collect (fun (_, xs) -> xs |> Seq.combinePairs |> Seq.collect antiNodes)
        |> Seq.distinct
        |> Seq.filter bounds.Contains
        |> Seq.length
        |> Answer.submit

    [<Test>]
    let Part2 () =
        let scan i j =
            List.unfold (fun x -> if bounds.Contains(x) then Some(x, x + (i - j)) else None) i

        let antiNodes (a, b) = List.append (scan a b) (scan b a)

        nodes
        |> Seq.collect (fun (_, xs) -> xs |> Seq.combinePairs |> Seq.collect antiNodes)
        |> Seq.distinct
        |> Seq.filter bounds.Contains
        |> Seq.length
        |> Answer.submit
