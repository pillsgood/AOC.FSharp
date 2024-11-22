namespace AOC.FSharp.Y2023

open AOC.FSharp.Common
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day11 =
    let input: string[] = Input.fetch
    let dim = int2 (input |> Array.head |> _.Length, input |> Array.length)

    let map: int2 list =
        let map2d f xs =
            xs
            |> Seq.indexed
            |> Seq.collect (fun (j, str) -> str |> Seq.indexed |> Seq.choose (fun (i, c) -> f ((i, j), c)))

        input
        |> map2d (function
            | (i, j), '#' -> Some(int2 (i, j))
            | _ -> None)
        |> Seq.toList

    let findExpansions (c: vector2.Component) =
        let set =
            Seq.init dim[c] id
            |> Seq.filter (fun i -> map |> Seq.forall (fun v -> v[c] != i))
            |> Seq.toList

        Seq.init dim[c] id
        |> Seq.map (fun i -> i, set |> Seq.filter (fun j -> j < i) |> Seq.length)
        |> Map

    let expansion = {| x = findExpansions vector2.Component.x; y = findExpansions vector2.Component.y |}

    [<Test>]
    let Part1 () =
        let expand (position: int2) =
            let x = expansion.x |> Map.find position.x
            let y = expansion.y |> Map.find position.y
            position + int2 (x, y)

        let map = map |> List.map expand

        let getPairs (map: 'a list) : ('a * 'a) seq =
            map
            |> Seq.indexed
            |> Seq.collect (fun (i, v) -> ([ v ], map[(i + 1) ..]) ||> Seq.allPairs)

        getPairs map
        |> Seq.sumBy (fun (l, r) -> vector.manhattan (l - r))
        |> Answer.submit


    [<Test>]
    let Part2 () =
        let expand (position: int2) =
            let x = expansion.x |> Map.find position.x
            let y = expansion.y |> Map.find position.y
            position + (int2 (x, y) * 999999)

        let map = map |> List.map expand

        let getPairs (map: 'a list) : ('a * 'a) seq =
            map
            |> Seq.indexed
            |> Seq.collect (fun (i, v) -> ([ v ], map[(i + 1) ..]) ||> Seq.allPairs)

        getPairs map
        |> Seq.sumBy (fun (l, r) -> int64 (vector.manhattan (l - r)))
        |> Answer.submit
