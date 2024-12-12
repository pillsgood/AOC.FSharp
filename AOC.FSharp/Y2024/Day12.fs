namespace AOC.FSharp.Y2024

open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day12 =
    let tiles =
        Input.fetch<string array>
        |> Array.rev
        |> Array.map2d (fun i j c -> int2 (i, j), c)
        |> Array.toList

    let map = tiles |> List.map (fun t -> fst t, t) |> Map

    type Tile = int2 * char
    let getNeighbors position = int2.cardinalDirections |> Array.map ((+) position)

    let getRegion tile : (int2 * char) list =
        let getNeighbors (position, rid) (visited: Set<_>) =
            getNeighbors position
            |> Seq.choose map.TryFind
            |> Seq.filter (snd >> (=) rid)
            |> Seq.filter (not << visited.Contains)
            |> Seq.toList

        let rec search (scope: Tile list) (visited: Tile Set) acc =
            match scope with
            | [] -> acc
            | cur :: rem ->
                let visited = visited |> Set.add cur
                let scope = getNeighbors cur visited @ rem |> List.except visited
                search scope visited (cur :: acc)

        search [ tile ] Set.empty []

    let rec scanRegions tiles =
        match Seq.tryHead tiles with
        | Some(tile) ->
            getRegion tile
            |> fun region -> scanRegions (tiles |> List.except region) @ [ region ]
        | _ -> []

    let isOutsideRegion rid tile = not (tile |> Option.exists (snd >> (=) rid))

    let measurePerimeter region =
        region
        |> List.sumBy (fun (p, rid) -> getNeighbors p |> Seq.count (map.TryFind >> (isOutsideRegion rid)))

    let measureSides (region: Tile list) =
        let inline convex ((position, rid): Tile) =
            Seq.init 4 (fun i ->
                [ int2.cardinalDirections[i]; int2.cardinalDirections[(i + 1) % 4] ]
                |> List.map ((+) position)
                |> List.map map.TryFind)
            |> Seq.count (fun scan -> scan |> List.forall (isOutsideRegion rid))

        let inline concave ((position, rid): Tile) =
            Seq.init 4 (fun i ->
                [ int2.cardinalDirections[i]; int2.cardinalDirections[(i + 1) % 4] ]
                |> List.map ((+) position)
                |> List.choose map.TryFind,
                map.TryFind(int2.ordinalDirections[i] + position))
            |> Seq.count (fun scan ->
                match scan with
                | [ (_, a); (_, b) ], diagonal -> a = rid && b = rid && (diagonal |> (isOutsideRegion rid))
                | _ -> false)

        region |> Seq.sumBy (fun t -> concave t + convex t)

    [<Test>]
    let Part1 () =
        scanRegions tiles
        |> Seq.sumBy (fun region -> (measurePerimeter region) * (List.length region))
        |> Answer.submit

    [<Test>]
    let Part2 () =
        scanRegions tiles
        |> Seq.sumBy (fun region -> (measureSides region) * (List.length region))
        |> Answer.submit
