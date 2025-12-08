namespace AOC.FSharp.Y2025

open AOC.FSharp.Common
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day08 =
    let input =
        Input.fetch<string array>
        |> Array.choose (
            String.split ","
            >> (function
            | [| x; y; z |] -> Some(float3 (float32 x, float32 y, float32 z))
            | _ -> None)
        )

    let junctions =
        input
        |> Seq.combinePairs
        |> Seq.sortBy (fun (a, b) -> Vector.distance a b)
        |> Seq.toList

    let createCircuit (groups: float3 Set list) (a: float3, b: float3) =
        let matchingGroups, otherGroups =
            groups
            |> List.partition (fun group -> group |> Set.exists (fun x -> x = a || x = b))

        let mergedGroup = matchingGroups |> List.fold Set.union (Set.ofList [ a; b ])
        mergedGroup :: otherGroups

    [<Test>]
    let Part1 () =
        let circuits = junctions |> Seq.take 1000 |> Seq.fold createCircuit []

        circuits
        |> (Seq.map _.Count >> Seq.sortDescending)
        |> (Seq.take 3 >> Seq.reduce (*))
        |> Answer.submit

    [<Test>]
    let Part2 () =
        let rec findFinalConnection (items: (float3 * float3) list) (acc: float3 Set list) =
            match items with
            | head :: tail ->
                createCircuit acc head
                |> function
                    | [ s ] when s.Count = input.Length -> head
                    | acc' -> findFinalConnection tail acc'
            | _ -> failwith "unreachable"

        let a, b = findFinalConnection junctions []
        Answer.submit ((int64 a.x) * (int64 b.x))
