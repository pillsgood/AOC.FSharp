namespace AOC.FSharp.Y2024

open Microsoft.FSharp.Collections
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day05 =
    let input: string array = Input.fetch

    let rules =
        input
        |> Array.filter _.Contains("|")
        |> Array.map ((String.split "|") >> (Array.map int))
        |> Seq.collect (fun c -> [ ((c[0], c[1]), -1); ((c[1], c[0]), 1) ])
        |> Map

    let updates =
        input
        |> Array.filter _.Contains(",")
        |> Array.map (String.split "," >> (Seq.map int) >> Seq.toList)

    let sort pages =
        pages
        |> List.sortWith (fun a b -> rules |> Map.tryFind (a, b) |> Option.defaultValue 0)

    [<Test>]
    let Part1 () =
        updates
        |> Seq.filter (fun pages -> sort pages |> (=) pages)
        |> Seq.sumBy (fun pages -> pages[pages.Length / 2])
        |> Answer.submit

    [<Test>]
    let Part2 () =
        updates
        |> Seq.map (fun pages -> pages, sort pages)
        |> Seq.filter (fun (a, b) -> a <> b)
        |> Seq.sumBy (fun (_, b) -> b[b.Length / 2])
        |> Answer.submit
