namespace AOC.FSharp.Y2024

open System
open System.Collections.Generic
open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day16 =
    type Tile =
        | Start
        | Goal
        | Obstacle
        | Path

    let map, bounds =
        let input = Input.fetch<string array>

        let map =
            input
            |> Array.rev
            |> Array.map2d (fun i j c -> int2 (i, j), c)
            |> Array.choose (function
                | p, 'S' -> Some(p, Start)
                | p, 'E' -> Some(p, Goal)
                | p, '#' -> Some(p, Obstacle)
                | p, '.' -> Some(p, Path)
                | _ -> None)
            |> Map.ofArray

        map, input |> Array.dimensions

    let start = map |> Map.findKey (fun _ t -> t.IsStart)
    let goal = map |> Map.findKey (fun _ t -> t.IsGoal)

    let neighbors pos =
        int2.cardinalDirections
        |> Seq.map (fun v -> v + pos)
        |> Seq.filter (map.TryFind >> (not << Option.exists _.IsObstacle))

    let adjacent (pos: int2, dir: int2) =
        let inline rotateRight (d: int2) = int2 (d.y, -d.x)
        let inline rotateLeft (d: int2) = int2 (-d.y, d.x)

        [ (pos + dir, dir), 1
          (pos, rotateRight dir), 1000
          (pos, rotateLeft dir), 1000 ]
        |> List.filter (fun ((p, _), _) -> map.TryFind p |> (not << Option.exists _.IsObstacle))

    let adjacentRev (pos: int2, dir: int2) =
        let inline rotateRight (d: int2) = int2 (d.y, -d.x)
        let inline rotateLeft (d: int2) = int2 (-d.y, d.x)

        [ (pos - dir, dir), 1
          (pos, rotateRight dir), 1000
          (pos, rotateLeft dir), 1000 ]
        |> List.filter (fun ((p, _), _) -> map.TryFind p |> (not << Option.exists _.IsObstacle))

    let search (start: int2) (goal: int2) =

        let graph = Array3D.create bounds.x bounds.y 4 {| visited = false; cost = Int32.MaxValue |}
        let queue = PriorityQueue()

        let inline indexOf v = Array.IndexOf(int2.cardinalDirections, v)
        let inline findCost (pos: int2, dir: int2) = graph[pos.x, pos.y, indexOf dir].cost
        let inline visited (pos: int2, dir: int2) = graph[pos.x, pos.y, indexOf dir].visited

        let inline setVisit (pos: int2, dir: int2) =
            let mutable n = &graph[pos.x, pos.y, indexOf dir]
            n <- {| n with visited = true |}

        let inline setCost cost (pos: int2, dir: int2) =
            let mutable n = &graph[pos.x, pos.y, indexOf dir]
            n <- {| n with cost = cost |}

        let rec scan () =
            if queue.Count = 0 then
                int2.cardinalDirections
                |> Seq.mapMin (fun dir -> findCost (goal, dir))
                |> fun minCost -> graph, minCost
            else
                match queue.Dequeue() with
                | node when visited node -> scan ()
                | node ->
                    setVisit node

                    adjacent node
                    |> List.filter (fun (n, _) -> not (visited n))
                    |> List.map (fun (n, cost) -> n, findCost node + cost)
                    |> List.filter (fun (n, cost) -> cost <= findCost n)
                    |> List.iter (fun (n, cost) ->
                        n |> setCost cost
                        queue.Enqueue(n, cost))

                    scan ()

        setCost 0 (start, int2.right)
        queue.Enqueue((start, int2.right), 0)
        scan ()

    let backtrack (graph: int array3d) points =
        let inline indexOf v = Array.IndexOf(int2.cardinalDirections, v)
        let inline findCost (pos: int2, dir: int2) = graph[pos.x, pos.y, indexOf dir]

        let rec scan visited rem =
            match rem with
            | [] -> visited
            | h :: t when visited |> Set.contains h -> scan visited t
            | (pos, dir) :: t ->
                adjacentRev (pos, dir)
                |> List.filter (fun (n, cost) -> findCost (pos, dir) - findCost n = cost)
                |> List.map fst
                |> fun ns -> ns @ t
                |> scan (visited |> Set.add (pos, dir))

        scan Set.empty (points |> Seq.toList)

    [<Test>]
    let Part1 () = search start goal |> snd |> Answer.submit

    [<Test>]
    let Part2 () =
        let graph, _ = search start goal

        int2.cardinalDirections
        |> Seq.map (fun dir -> goal, dir)
        |> backtrack (graph |> Array3D.map _.cost)
        |> Set.map fst
        |> Set.count
        |> Answer.submit
