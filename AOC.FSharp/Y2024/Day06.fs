namespace AOC.FSharp.Y2024

open System.Collections.Generic
open System.Linq
open AOC.FSharp.Common
open FSharp.Collections.ParallelSeq
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
            let set = HashSet<_>()

            (pos, dir)
            |> traverse (obstacles |> Set.add (pos + dir))
            |> Seq.takeUntil set.Add
            |> Seq.countBy id
            |> Seq.exists (fun (_, c) -> c > 1)

        (start, int2.up)
        |> traverse obstacles
        |> Seq.filter (fun (pos, dir) ->
            let obs = pos + dir
            not (obstacles.Contains(obs)) && bounds |> Rect.contains obs)
        |> Seq.map (fun (pos, dir) -> pos + dir, canLoop (pos, dir))
        |> Seq.distinctBy fst
        |> Seq.count snd
        |> Answer.submit
