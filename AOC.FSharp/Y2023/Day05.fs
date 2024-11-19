namespace AOC.FSharp.Y2023

open System.Text.RegularExpressions
open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

type Range = { Start: int64; End: int64 }

type MapDefinition =
    { SrcName: string
      DstName: string
      Instructions: MapInstruction list }

and MapInstruction = { Range: Range; Offset: int64 }

[<TestFixture>]
type Day05() =
    inherit AocFixture()

    let input = base.Input.Get<string[]>()

    let maps =
        let input = input[1..] |> String.concat "\n"

        let pattern =
            Regex(@"(?<source>\w+)-to-(?<destination>\w+) map\:\n(?:(?<dstRange>\d+) (?<srcRange>\d+) (?<length>\d+)\n?)+")

        match input with
        | Matches pattern matches ->
            [ for m in matches ->
                  { SrcName = m.Groups["source"].Value
                    DstName = m.Groups["destination"].Value
                    Instructions =
                      [ for i in 0 .. m.Groups["dstRange"].Captures.Count - 1 ->
                            let src = int64 m.Groups["srcRange"].Captures[i].Value
                            let dst = int64 m.Groups["dstRange"].Captures[i].Value
                            let len = int64 m.Groups["length"].Captures[i].Value
                            { Range = { Start = src; End = src + len - 1L }; Offset = dst - src } ] } ]
        | _ -> []

    let start = maps |> Seq.tryFind (fun m -> m.SrcName = "seed")

    [<Test>]
    member _.Part1() =
        let seeds =
            match input[0] with
            | Matches (Regex(@"\d+")) parts -> parts |> List.map (_.Value >> int64)
            | _ -> []

        let rec traverse (map: MapDefinition option) (seed: int64) =
            match map with
            | Some source ->
                let next = maps |> Seq.tryFind (fun m -> m.SrcName = source.DstName)

                let range =
                    source.Instructions
                    |> Seq.tryFind (fun i -> seed >= i.Range.Start && seed <= i.Range.End)

                let seed =
                    match range with
                    | Some i -> seed + i.Offset
                    | None -> seed

                traverse next seed

            | None -> seed

        seeds |> Seq.minMap (traverse start) |> base.Answer.Submit

    [<Test>]
    member _.Part2() =
        let intersects src dst = src.End > dst.Start && src.Start < dst.End

        let split (instruction: MapInstruction) (src: Range) : (Range * Range option) option =
            let dst = instruction.Range

            let (|Before|_|) src = src.Start < dst.Start && src.End >= dst.Start
            let (|Overlap|_|) src = src.End > dst.Start && src.Start < dst.End
            let (|After|_|) src = src.End > dst.End && src.Start >= dst.End

            match src with
            | Before -> Some({ src with End = dst.Start - 1L }, Some { dst with End = src.End })
            | Overlap ->
                let offset range =
                    { Start = range.Start + instruction.Offset; End = range.End + instruction.Offset }

                let segment = { Start = (max src.Start dst.Start); End = (min src.End dst.End) }
                let remainder = (segment.End < src.End) ?-> { Start = segment.End + 1L; End = src.End }

                Some(offset segment, remainder)
            | After -> Some({ src with Start = dst.End + 1L }, None)
            | _ -> Some(src, None)

        let split (def: MapDefinition) (range: Range) =
            let instructions =
                def.Instructions
                |> List.filter (_.Range >> intersects range)
                |> List.sortBy _.Range.Start

            Some range
            |> Seq.unfold (fun range ->
                match range with
                | Some range ->
                    match (instructions |> Seq.tryFind (_.Range >> intersects range)) with
                    | Some map -> split map range
                    | None -> Some(range, None)
                | None -> None)

        let rec traverse (map: MapDefinition option) (range: Range) : Range seq =
            match map with
            | Some source ->
                let next = maps |> Seq.tryFind (fun m -> m.SrcName = source.DstName)
                split source range |> Seq.collect (traverse next)
            | None -> Seq.singleton range

        let ranges =
            match input[0] with
            | Matches (Regex(@"\d+")) parts ->
                parts
                |> Seq.map (_.Value >> int64)
                |> Seq.chunkBySize 2
                |> Seq.choose (function
                    | [| start; len |] -> Some { Start = start; End = start + len - 1L }
                    | _ -> None)
            | _ -> []

        ranges
        |> Seq.collect (traverse start)
        |> Seq.minMap _.Start
        |> base.Answer.Submit
