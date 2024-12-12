namespace AOC.FSharp.Y2024

open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day12 =
    module Input =
        let fetch<'u> =
            """
            AAAA
            BBCD
            BBCC
            EEEC
            """
            |> String.splitLines

    let tiles =
        Input.fetch<string array>
        |> Array.rev
        |> Array.map2d (fun i j c -> int2 (i, j), c)

    let map = tiles |> Array.map (fun t -> fst t, t) |> Map

    type Tile = int2 * char
    let getNeighbors position = int2.cardinalDirections |> Array.map ((+) position)

    let getRegion tile : (int2 * char) list =
        let getNeighbors tile (visited: Set<_>) =
            getNeighbors (fst tile)
            |> Seq.choose map.TryFind
            |> Seq.filter (snd >> (=) (snd tile))
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

    [<Test>]
    let Part1 () =
        let rec scanRegions tiles =
            match Seq.tryHead tiles with
            | Some(tile) ->
                getRegion tile
                |> fun region -> scanRegions (tiles |> List.except region) @ [ region ]
            | _ -> []

        let measure (region: Tile list) =
            let perimeter =
                region
                |> List.sumBy (fun (p, rid) -> getNeighbors p |> Seq.count (Option.exists ((<>) rid)))
            perimeter * (List.length region)

        scanRegions (tiles |> List.ofArray) |> Seq.sumBy measure |> printfn "%A"

    [<Test>]
    let Part2 () = ()
