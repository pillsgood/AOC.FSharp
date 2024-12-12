namespace AOC.FSharp.Y2024

open System.Collections.Generic
open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day12 =
    // module Input =
    //     let fetch =
    //         """
    //         RRRRIICCFF
    //         RRRRIICCCF
    //         VVRRRCCFFF
    //         VVRCCCJFFF
    //         VVVVCJJCFE
    //         VVIVCCJJEE
    //         VVIIICJJEE
    //         MIIIIIJJEE
    //         MIIISIJEEE
    //         MMMISSJEEE
    //         """
    //         |> String.splitLines

    let tiles =
        Input.fetch<string array>
        |> Array.rev
        |> Array.map2d (fun i j c -> int2 (i, j), c)

    let map = tiles |> Array.map (fun t -> fst t, t) |> Map
    let cache = HashSet()

    [<Struct>]
    type Region =
        { Id: char
          Area: int
          Perimeter: int }

    let getRegion (position, regionId) : Region option =
        let scope = HashSet()

        let rec fill position (region: Region) =
            scope.Add(position) |> ignore
            cache.Add(position) |> ignore

            let neighbors =
                int2.cardinalDirections
                |> Array.map ((+) position)
                |> Array.map (fun v -> v, map.TryFind v)

            let inline addPerimeter r = { r with Perimeter = r.Perimeter + 1 }
            let inline addArea position r = fill position { r with Area = r.Area + 1 }

            (region, neighbors)
            ||> Array.fold (fun acc (p, opt) ->
                let inline step opt =
                    match opt with
                    | Some(v, c) -> if c = regionId then (addArea v) else addPerimeter
                    | None -> addPerimeter

                acc |> (if scope.Contains p then id else step opt))

        let region =
            { Id = regionId
              Area = 1
              Perimeter = 0 }

        if cache.Contains(position) then None else Some(fill position region)

    [<Test>]
    let Part1 () =
        tiles
        |> Array.choose getRegion
        |> Array.sumBy (fun region -> region.Area * region.Perimeter)
        |> Answer.submit

    [<Test>]
    let Part2 () = ()
