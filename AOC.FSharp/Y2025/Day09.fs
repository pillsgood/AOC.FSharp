namespace AOC.FSharp.Y2025

open AOC.FSharp.Common
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day09 =
    type int2 = vector2<int64>
    type rectInt = RectInt<int64>

    let input =
        Input.fetch
        |> Array.choose (
            String.split ","
            >> function
                | [| x; y |] -> Some(int2 (int x, int y))
                | _ -> None
        )
        |> Array.sortBy (fun v -> v.y, v.x)

    let createRect (a: int2, b: int2) = rectInt (min a.x b.x, min a.y b.y, max a.x b.x, max a.y b.y)

    [<Test>]
    let Part1 () =
        input
        |> Seq.combine2
        |> Seq.map createRect
        |> Seq.findMax RectInt.area
        |> Answer.submit

    [<Test>]
    let Part2 () =
        let lines =
            input
            |> Seq.combinePairs
            |> Seq.filter (fun (a, b) -> a.x = b.x || a.y = b.y)
            |> Seq.sortBy (fun (a, b) -> Vector.manhattan (a - b), b, a)
            |> Seq.map createRect
            |> Seq.toList

        input
        |> Seq.combine2
        |> Seq.map createRect
        |> Seq.filter (fun rect ->
            lines
            |> List.forall (not << (RectInt.intersects (rect |> RectInt.shrink int2.one))))
        |> Seq.findMax RectInt.area
        |> Answer.submit
