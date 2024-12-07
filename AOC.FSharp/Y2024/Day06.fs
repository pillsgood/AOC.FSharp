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
    let bounds = rectInt (int2.zero, Array.dimensions input)

    let map =
        input
        |> Seq.mapi (fun j str -> str |> Seq.mapi (fun i c -> int2 (i, bounds.height - 1 - j), c))
        |> Seq.concat

    let obstacles = map |> Seq.filter (snd >> (=) '#') |> Seq.map fst |> Set
    let start = map |> Seq.find (snd >> (=) '^') |> fst
    let rotateRight (v: int2) = int2 (v.y, -v.x)

    let traverse obstacles =
        Seq.unfold (fun (pos, dir) ->
            let inBounds = bounds |> Rect.contains pos
            let blocked = obstacles |> Set.contains (pos + dir)

            match (inBounds, blocked) with
            | true, true -> Some((pos, dir), (pos, rotateRight dir))
            | true, false -> Some((pos, dir), (pos + dir, dir))
            | _ -> None)

    [<Test>]
    let Part1 () =
        (start, int2.up)
        |> traverse obstacles
        |> Seq.distinctBy fst
        |> Seq.length
        |> Answer.submit

    [<Test>]
    let Part2 () =
        let canLoop (pos, dir) =
            (pos, dir)
            |> traverse (obstacles |> Set.add (pos + dir))
            |> Seq.takeUntil (HashSet<_>()).Add
            |> Seq.countBy id
            |> Seq.exists (fun (_, c) -> c > 1)

        let path =
            (start, int2.up)
            |> traverse obstacles
            |> Seq.filter (fun (pos, dir) ->
                pos + dir
                |> fun obs -> not (obstacles.Contains(obs)) && bounds |> Rect.contains obs)
            |> Seq.distinctBy (fun (p, d) -> p + d)
            |> Seq.toArray

        path |> Array.Parallel.filter canLoop |> Array.length |> Answer.submit
