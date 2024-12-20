namespace AOC.FSharp.Y2024

open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day20 =
    [<Struct>]
    type Tile =
        | Start of pos: int2
        | Goal of pos: int2
        | Track of pos: int2

        member this.position =
            match this with
            | Start p -> p
            | Goal p -> p
            | Track p -> p

    let map =
        Input.fetch<string array>
        |> Array.rev
        |> Array.choose2d (fun i j c ->
            int2 (i, j)
            |> fun p ->
                match c with
                | 'S' -> Some(p, Start p)
                | 'E' -> Some(p, Goal p)
                | '.' -> Some(p, Track p)
                | _ -> None)
        |> Map.ofArray

    let adjacent (tile: Tile) =
        int2.cardinalDirections
        |> Seq.choose (fun p -> map |> Map.tryFind (tile.position + p))

    let path =
        let start = map |> Map.findValue _.IsStart

        (Some start, adjacent start |> Seq.tryHead)
        |> Array.unfold (fun (current, next) ->
            match current with
            | None -> None
            | Some current ->
                match next with
                | Some next ->
                    let nextAdjacent = adjacent next |> Seq.filter (fun t -> not (t = current))
                    Some(current, (Some next, nextAdjacent |> Seq.tryHead))
                | None -> Some(current, (None, None)))

    let costs = path |> Array.indexed |> Array.map (fun (i, t) -> t.position, i) |> Map.ofArray

    [<Test>]
    let Part1 () =
        let findShortcut tile cost =
            int2.cardinalDirections
            |> Seq.map (fun v -> costs |> Map.tryFind (v + tile), costs |> Map.tryFind ((v * 2) + tile))
            |> Seq.choose (fun (a, b) ->
                match a, b with
                | None, Some c when c > cost -> Some((c - cost) - 2)
                | _ -> None)

        path
        |> Seq.collect (fun t -> costs[t.position] |> findShortcut t.position)
        |> Seq.count (fun save -> save >= 100)
        |> Answer.submit

    [<Test>]
    let Part2 () =
        let countShortcut f (tile: int2, time: int) =
            path[(time + 100) ..]
            |> Array.sumBy (fun v ->
                let d = Vector.manhattan (v.position - tile)
                if d <= 20 && f ((costs[v.position] - time) - d) then 1 else 0)

        path
        |> Array.indexed
        |> Array.Parallel.sumBy (fun (time, t) -> (t.position, time) |> countShortcut (fun save -> save >= 100))
        |> Answer.submit
