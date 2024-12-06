namespace AOC.FSharp.Y2024

open System.Collections.Generic
open AOC.FSharp.Common
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day06 =
    let input: string array = Input.fetch
    // let input: string array =
    //     """
    //     ....#.....
    //     .........#
    //     ..........
    //     ..#.......
    //     .......#..
    //     ..........
    //     .#..^.....
    //     ........#.
    //     #.........
    //     ......#...
    //     """
    //     |> String.splitLines

    let dimensions = rectInt (int2.zero, Array.dimensions input)

    let map =
        input
        |> Seq.mapi (fun j str -> str |> Seq.mapi (fun i c -> int2 (i, dimensions.height - 1 - j), c))
        |> Seq.concat

    let obstacles = map |> Seq.filter (snd >> (=) '#') |> Seq.map fst |> Set
    let start = map |> Seq.find (snd >> (=) '^') |> fst

    let rotateRight (v: int2) = int2 (v.y, -v.x)

    [<Test>]
    let Part1 () =
        let traverse (pos, dir) =
            let dir =
                obstacles
                |> Set.contains (pos + dir)
                |> (fun blocked -> if blocked then rotateRight else id)
                <| dir

            match dimensions |> Rect.contains pos with
            | true -> Some(pos, (pos + dir, dir))
            | false -> None

        (start, int2.up)
        |> Seq.unfold traverse
        |> Seq.distinct
        |> Seq.length
        |> Answer.submit


    [<Test>]
    let Part2 () =
        let willLoop pos dir =
            let obstacles = obstacles |> Set.add (pos + dir)

            let hitScan pos dir =
                obstacles
                |> Seq.filter (fun obs -> Vector.normalize (obs - pos) = dir)
                |> Seq.sortBy (fun v -> Vector.manhattan (v - pos))
                |> Seq.tryHead
                |> Option.map (fun x -> x - dir, dir)

            let mutable visited = HashSet()

            let turns =
                (pos, dir)
                |> Seq.unfold (fun (pos, dir) -> hitScan pos (rotateRight dir) |> Option.map (fun t -> (pos, dir), t))
                |> Seq.takeUntil (fun (p, _) -> visited.Add(p))
                |> Seq.map fst
                |> Seq.toList

            let head = (turns |> List.tryHead)
            let tail = (turns |> List.tryLast)
            turns.Length > 1 && head = tail

        let traverse (pos, dir) =
            match dimensions |> Rect.contains (pos + dir) with
            | true ->
                let blocked = obstacles |> Set.contains (pos + dir)
                let dir = dir |> (if blocked then rotateRight else id)
                let looping = willLoop pos dir
                Some((pos, looping), (pos + dir, dir))
            | false -> None

        (start, int2.up)
        |> Seq.unfold traverse
        |> Seq.filter snd
        |> Seq.distinct
        |> Seq.filter (fun (x, _) -> x <> start)
        |> Seq.length
        |> (fun x -> printfn $"{x}")
