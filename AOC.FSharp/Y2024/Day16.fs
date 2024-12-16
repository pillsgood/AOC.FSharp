namespace AOC.FSharp.Y2024

open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day16 =
    //     module Input =
    //         let fetch<'u> =
    //             """
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
    //             |> String.splitLines

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

    let heuristicFn (a: int2) (b: int2) = Vector.manhattan (a - b)

    let costFn (a: int2, dir: int2) (b: int2) =
        b - a
        |> fun d ->
            if d = dir then 1
            else if d = -dir then 2001
            else 1001

    let neighbors a =
        int2.cardinalDirections
        |> Seq.map (fun v -> v + a)
        |> Seq.filter (map.TryFind >> (not << Option.exists _.IsObstacle))

    let search (start: int2) (goal: int2) : seq<int2> option =
        let rec reconstructPath cameFrom current =
            seq {
                yield current

                match Map.tryFind current cameFrom with
                | None -> ()
                | Some next -> yield! reconstructPath cameFrom next
            }

        let rec scan closedSet (openSet, gScores, fScores, cameFrom) =
            match List.sortBy (fun n -> Map.find n fScores) openSet with
            | current :: _ when current = goal -> Some(reconstructPath cameFrom current |> Seq.rev)
            | current :: rest ->
                let gScore = Map.find current gScores

                let next =
                    neighbors current
                    |> Seq.filter (fun n -> closedSet |> Set.contains n |> not)
                    |> Seq.fold
                        (fun (openSet, gScores, fScores, cameFrom) neighbour ->
                            let dir =
                                cameFrom
                                |> Map.tryFind current
                                |> Option.map (fun previous -> current - previous)
                                |> Option.defaultValue int2.right

                            let tentativeGScore = gScore + costFn (current, dir) neighbour

                            if List.contains neighbour openSet && tentativeGScore >= Map.find neighbour gScores then
                                (openSet, gScores, fScores, cameFrom)
                            else
                                let newOpenSet =
                                    if List.contains neighbour openSet then openSet else neighbour :: openSet

                                let newGScores = Map.add neighbour tentativeGScore gScores

                                let newFScores =
                                    Map.add neighbour (tentativeGScore + heuristicFn neighbour goal) fScores

                                let newCameFrom = Map.add neighbour current cameFrom
                                newOpenSet, newGScores, newFScores, newCameFrom)
                        (rest, gScores, fScores, cameFrom)

                scan (Set.add current closedSet) next
            | _ -> None

        let gScores = Map.ofList [ start, 0 ]
        let fScores = Map.ofList [ start, heuristicFn start goal ]

        scan Set.empty ([ start ], gScores, fScores, Map.empty)

    let dump path =
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

    [<Test>]
    let Part1 () =
        let start = map |> Map.findKey (fun _ t -> t.IsStart)
        let goal = map |> Map.findKey (fun _ t -> t.IsGoal)

        let path = search start goal |> Option.get |> List.ofSeq

        path
        |> Seq.skip 1
        |> Seq.fold
            (fun (a, dir, score) b ->
                let score = score + (costFn (a, dir) b)
                (b, b - a, score))
            (start, int2.right, 0)
        |> fun (_, _, score) -> score
        |> Answer.submit

    [<Test>]
    let Part2 () = ()
