namespace AOC.FSharp.Y2024

open System.Collections.Generic
open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day18 =
    let input =
        Input.fetch<string array>
        |> Array.map (String.split "," >> Array.map int)
        |> Array.map (fun l -> int2 (l[0], l[1]))

    let bounds = rectInt (int2.zero, int2 (70, 70) + int2.one)

    let search (start: int2) (goal: int2) (map: int2 Set) =
        let graph = Array2D.create bounds.size.x bounds.size.y struct {| visited = false; cost = None |}
        let queue = PriorityQueue()

        let findCost (pos: int2) = graph[pos.x, pos.y] |> _.cost
        let visited (pos: int2) = graph[pos.x, pos.y] |> _.visited

        let setVisited (pos: int2) =
            let mutable n = &graph[pos.x, pos.y]
            n <- {| n with visited = true |}

        let setCost cost (pos: int2) =
            let mutable n = &graph[pos.x, pos.y]
            n <- {| n with cost = Some cost |}

        let adjacent pos =
            int2.cardinalDirections
            |> Seq.map (fun v -> v + pos)
            |> Seq.filter (fun v -> not (map.Contains v) && bounds.Contains v)

        let rec scan () =
            match queue.TryDequeue() with
            | false, _, _ -> findCost goal
            | true, node, _ when visited node -> scan ()
            | true, node, _ ->
                setVisited node

                adjacent node
                |> Seq.filter (fun n -> not (visited n))
                |> Seq.choose (fun n -> findCost node |> Option.map (fun cost -> (n, cost + 1)))
                |> Seq.filter (fun (n, cost) -> findCost n |> Option.forall (fun previous -> cost <= previous))
                |> Seq.iter (fun (n, cost) ->
                    n |> setCost cost
                    queue.Enqueue(n, cost))

                scan ()

        setCost 0 start
        queue.Enqueue(start, 0)
        scan ()

    [<Test>]
    let Part1 () =
        let map = input |> Array.take 1024 |> Set
        let start, goal = bounds.min, bounds.max
        map |> search start goal |> Option.get |> Answer.submit

    [<Test>]
    let Part2 () =
        let start, goal = bounds.min, bounds.max
        let run i = input |> Array.take i |> Set |> search start goal

        let findIteration () =
            let rec binarySearch low high =
                if low > high then
                    Some(low - 1)
                else
                    let mid = low + (high - low) / 2

                    match run mid with
                    | None -> binarySearch low (mid - 1)
                    | _ -> binarySearch (mid + 1) high

            binarySearch 1024 input.Length

        findIteration ()
        |> Option.map (fun i -> input[i] |> fun v -> $"{v.x},{v.y}")
        |> Option.get
        |> Answer.submit
