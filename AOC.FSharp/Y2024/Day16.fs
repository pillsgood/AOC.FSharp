namespace AOC.FSharp.Y2024

open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day16 =
    // module Input =
    //     let fetch<'u> =
    //         """
    // ###############
    // #.......#....E#
    // #.#.###.#.###.#
    // #.....#.#...#.#
    // #.###.#####.#.#
    // #.#.#.......#.#
    // #.#.#####.###.#
    // #...........#.#
    // ###.#.#####.#.#
    // #...#.....#.#.#
    // #.#.#.###.#.#.#
    // #.....#...#.#.#
    // #.###.#.#.#.#.#
    // #S..#.....#...#
    // ###############
    // """
    //         |> String.splitLines

    type Tile =
        | Start
        | Goal
        | Obstacle
        | Path

    let map =
        Input.fetch<string array>
        |> Array.rev
        |> Array.map2d (fun i j c -> int2 (i, j), c)
        |> Array.choose (function
            | p, 'S' -> Some(p, Start)
            | p, 'E' -> Some(p, Goal)
            | p, '#' -> Some(p, Obstacle)
            | p, '.' -> Some(p, Path)
            | _ -> None)
        |> Map.ofArray

    let start = map |> Map.findKey (fun _ t -> t.IsStart)
    let goal = map |> Map.findKey (fun _ t -> t.IsGoal)

    let heuristicFn (a: int2) (b: int2) = Vector.manhattan (a - b)

    let costFn (a: int2, dir: int2) (b: int2) =
        b - a
        |> fun d ->
            if d = dir then 1
            elif d = -dir then 2001
            else 1001

    let neighbors a =
        int2.cardinalDirections
        |> Seq.map (fun v -> v + a)
        |> Seq.filter (map.TryFind >> (not << Option.exists _.IsObstacle))

    let search (start: int2) (goal: int2) =
        let mutable paths = [ Some(0, [ start ]) ]

        let scanFinished paths =
            paths
            |> List.exists (fun x ->
                match x with
                | Some(_, p :: _) -> p = goal
                | _ -> false)

        let searchPaths () : bool =
            let newPaths =
                paths
                |> List.collect (fun pathOption ->
                    match pathOption with
                    | None -> []
                    | Some(_, cur :: _) when cur = goal -> List.singleton pathOption
                    | Some(score, _) when score > 127520 -> []
                    | Some(score, path) ->
                        let current = List.head path
                        let dir = if List.length path > 1 then current - List.item 1 path else int2.right

                        let next =
                            neighbors current
                            |> Seq.map (fun neighbor ->
                                let newScore = score + costFn (current, dir) neighbor
                                Some(newScore, neighbor :: path))
                            |> Seq.filter (fun option ->
                                match option with
                                | Some(_, newPath) -> not (List.contains (List.head newPath) (List.tail newPath))
                                | _ -> false)
                            |> Seq.toList

                        next)

            paths <- newPaths
            not (scanFinished paths)

        while searchPaths () do
            paths <- paths |> List.sortDescending

        let paths =
            paths
            |> List.choose (fun opt ->
                match opt with
                | Some(_, p :: _) when p = goal -> opt
                | _ -> None)

        let cost = paths |> List.minBy fst |> fst

        paths
        |> List.filter (fun (score, _) -> score = cost)
        |> List.map (fun (score, path) -> score, List.rev path)

    let printPath path =
        let dir v =
            if v = int2.up then '^'
            else if v = int2.right then '>'
            else if v = int2.down then 'v'
            else if v = int2.left then '<'
            else '?'

        let history = path |> List.pairwise |> List.map (fun (a, b) -> b, dir (b - a)) |> Map.ofList

        Input.fetch<string array>
        |> Array.rev
        |> Array.mapi (fun j str ->
            str
            |> String.mapi (fun i c ->
                match history.TryFind(int2 (i, j)) with
                | Some(d) -> d
                | None -> c))
        |> Array.rev
        |> String.concat "\n"
        |> fun str -> printfn $"{str}"

    let printVisited (points: int2 list) =
        Input.fetch<string array>
        |> Array.rev
        |> Array.mapi (fun j str ->
            str
            |> String.mapi (fun i c ->
                match points |> List.contains (int2 (i, j)) with
                | true -> 'O'
                | _ -> c))
        |> Array.rev
        |> String.concat "\n"
        |> fun str -> printfn $"{str}"

    [<Test>]
    let Part1 () =
        let path = search start goal
        path |> List.head |> snd |> printPath

    [<Test>]
    let Part2 () =
        let path = search start goal
        path |> List.collect snd |> List.distinct |> List.length |> Answer.submit
