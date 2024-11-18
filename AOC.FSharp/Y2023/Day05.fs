namespace AOC.FSharp.Y2023

open System
open System.Text.RegularExpressions
open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode
open FsUnit

type MapDefinition =
    { source: string
      destination: string
      instructions: MapInstruction list }

and MapInstruction =
    { srcName: string
      dstName: string
      src: int64
      dst: int64
      len: int64 }

type Range = { Start: int64; End: int64 }

[<TestFixture>]
type Day05() =
    inherit AocFixture()

    let input =
        """
        seeds: 79 14 55 13

        seed-to-soil map:
        50 98 2
        52 50 48

        soil-to-fertilizer map:
        0 15 37
        37 52 2
        39 0 15

        fertilizer-to-water map:
        49 53 8
        0 11 42
        42 0 7
        57 7 4

        water-to-light map:
        88 18 7
        18 25 70

        light-to-temperature map:
        45 77 23
        81 45 19
        68 64 13

        temperature-to-humidity map:
        0 69 1
        1 0 69

        humidity-to-location map:
        60 56 37
        56 93 4
        """
        |> String.splitLines

    // let input = base.Input.Get<string[]>()

    let maps =
        let input = input[1..] |> String.concat "\n"

        let pattern =
            Regex(@"(?<source>\w+)-to-(?<destination>\w+) map\:\n(?:(?<dstRange>\d+) (?<srcRange>\d+) (?<length>\d+)\n?)+")

        match input with
        | Matches pattern matches ->
            [ for m in matches ->
                  { source = m.Groups["source"].Value
                    destination = m.Groups["destination"].Value
                    instructions =
                      [ for i in 0 .. m.Groups["dstRange"].Captures.Count - 1 ->
                            { srcName = m.Groups["source"].Value
                              dstName = m.Groups["destination"].Value
                              src = int64 m.Groups["srcRange"].Captures[i].Value
                              dst = int64 m.Groups["dstRange"].Captures[i].Value
                              len = int64 m.Groups["length"].Captures[i].Value } ] } ]
        | _ -> []

    [<Test>]
    member _.Part1() =
        let seeds =
            match input[0] with
            | Matches (Regex(@"\d+")) parts -> parts |> List.map (_.Value >> int64)
            | _ -> []

        let rec traverse (map: MapDefinition option) (seed: int64) =
            match map with
            | Some source ->
                let next = maps |> Seq.tryFind (fun m -> m.source = source.destination)

                let range =
                    source.instructions
                    |> Seq.tryFind (fun i -> seed >= i.src && seed <= (i.src + i.len))

                let seed =
                    match range with
                    | Some i -> i.dst + (seed - i.src)
                    | None -> seed

                traverse next seed

            | None -> seed

        let start = maps |> Seq.tryFind (fun m -> m.source = "seed")
        seeds |> Seq.minMap (traverse start) |> base.Answer.Submit

    [<Test>]
    member _.Part2() =
        let intersects range map =
            range.End > map.src && range.Start < (map.src + map.len - 1L)

        let unfoldRange (maps: MapInstruction list) (source: Range option) : (Range * Range option) option =
            let splitRange (source: Range option) (map: MapInstruction) =
                let destination =
                    { Start = map.src
                      End = (map.src + map.len) - 1L }

                let (|Invalid|_|) range =
                    range.Start > range.End || (range.Start = 0 && range.End = 0)

                let (|Head|_|) range =
                    range.Start < destination.Start && range.End > destination.Start

                let (|Overlap|_|) range = range.Start < destination.End

                let (|Tail|_|) range = range.End >= destination.End

                match source with
                | Invalid -> None
                | Head ->
                    let segment =
                        { Start = source.Start
                          End = destination.Start - 1L }

                    let remainder =
                        { Start = destination.Start
                          End = source.End }

                    Some(segment, remainder)
                | Overlap ->
                    let offset = map.dst - map.src

                    let segment =
                        { Start = source.Start + offset
                          End = (min source.End destination.End) + offset }

                    let remainder =
                        { Start = segment.End + 1L
                          End = source.End }

                    Some(segment, remainder)
                | Tail ->
                    let segment =
                        { Start = source.Start + map.dst - map.src
                          End = source.End + map.dst - map.src }

                    Some(segment, { Start = 0L; End = 0L })
                | _ -> failwith $"Invalid range: {source}"

            maps
            |> List.tryFind (fun map ->
                match source with
                | Some r -> intersects r map
                | None -> false)
            |> splitRange source


        let split (def: MapDefinition) (range: Range) =
            let instructions =
                def.instructions |> List.filter (intersects range) |> List.sortBy _.src

            let result = Some range |> Seq.unfold (unfoldRange instructions) |> Seq.toList
            if List.isEmpty result then [ range ] else result

        let rec traverse (map: MapDefinition option) (range: Range) : Range seq =
            match map with
            | Some source ->
                let next = maps |> Seq.tryFind (fun m -> m.source = source.destination)
                split source range |> Seq.collect (traverse next) |> Seq.distinct
            | None -> Seq.singleton range

        let ranges =
            match input[0] with
            | Matches (Regex(@"\d+")) parts ->
                parts
                |> Seq.map (_.Value >> int64)
                |> Seq.chunkBySize 2
                |> Seq.choose (function
                    | [| start; len |] ->
                        Some
                            { Start = start
                              End = start + len - 1L }
                    | _ -> None)
            | _ -> []

        let start = maps |> Seq.tryFind (fun m -> m.source = "seed")
        let result = ranges |> Seq.collect (traverse start) |> Seq.distinct |> Seq.toList
        // result |> Seq.minMap _.start |> base.Answer.Submit
        result |> Seq.minMap _.Start |> should equal 46L
