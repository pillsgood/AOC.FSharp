namespace AOC.FSharp.Y2024

open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day20 =
    let map =
        Input.fetch<string array>
        |> Array.rev
        |> Array.choose2d (fun i j c ->
            int2 (i, j)
            |> fun p ->
                match c with
                | 'S' -> Some(p, (p, c))
                | 'E' -> Some(p, (p, c))
                | '.' -> Some(p, (p, c))
                | _ -> None)
        |> Map.ofArray

    let path =
        let adjacent (tile: int2) =
            int2.cardinalDirections
            |> Seq.choose (fun p -> map |> Map.tryFind (tile + p))
            |> Seq.map fst

        let start = map |> Map.findKey (fun _ (_, v) -> v = 'S')

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
        |> Array.indexed

    let lookup = path |> Array.map (fun (i, v) -> v, i) |> Map.ofArray

    [<Test>]
    let Part1 () =
        let findShortcut pos time =
            int2.cardinalDirections
            |> Seq.map (fun v -> lookup |> Map.tryFind (v + pos), lookup |> Map.tryFind ((v * 2) + pos))
            |> Seq.choose (fun (a, b) ->
                match a, b with
                | None, Some t when t > time -> Some((t - time) - 2)
                | _ -> None)

        path
        |> Seq.collect (fun (t, pos) -> findShortcut pos t)
        |> Seq.count (fun save -> save >= 100)
        |> Answer.submit

    let manhattanRadius distance =
        [ for x in -distance .. distance do
              let r = distance - abs x
              for y in -r .. r -> int2 (x, y) ]

    [<Test>]
    let Part2 () =
        let radius = manhattanRadius 20

        let countShortcut f (tile: int2, time: int) =
            radius
            |> Seq.choose (fun v ->
                v + tile
                |> fun v ->
                    match lookup.TryFind v with
                    | Some c -> Some(c, Vector.manhattan (v - tile))
                    | _ -> None)
            |> Seq.count (fun (t, d) -> f ((t - time) - d))

        path
        |> Array.Parallel.sumBy (fun (t, pos) -> (pos, t) |> countShortcut (fun save -> save >= 100))
        |> Answer.submit
