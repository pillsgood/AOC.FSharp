namespace AOC.FSharp.Y2024

open System
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day09 =

    let input: (int * int) array =
        Input.fetch<string>
        |> Seq.indexed
        |> Seq.map (fun (i, c) ->
            match i % 2 with
            | 1 -> (int (c - '0')), -1
            | _ -> (int (c - '0')), (i / 2))
        |> Seq.toArray

    let span input : int Span =
        Span(input |> Seq.collect (fun (size, id) -> Array.replicate size id) |> Seq.toArray)

    let inline swap i (blocks: int Span byref) : int =
        let mutable slice = blocks.Slice(i)
        match slice.LastIndexOfAnyExcept(-1) with
        | -1 -> -1
        | idx ->
            let result = slice[idx]
            slice[idx] <- -1
            result

    [<Test>]
    let Part1 () =
        let mutable blocks = span input
        let mutable checksum = 0L

        for i in 0 .. blocks.Length - 1 do
            if blocks[i] = -1 then blocks[i] <- (swap i &blocks)
            if blocks[i] <> -1 then checksum <- checksum + int64 (i * blocks[i])

        checksum |> Answer.submit

    [<Test>]
    let Part2 () =
        let mutable blocks: int Span = span input
        let mutable files = Span(input |> Array.filter (snd >> (<>) -1) |> Array.map fst)
        let mutable freeSpaces = Span(input |> Array.filter (snd >> (=) -1) |> Array.map fst)

        for fileId = files.Length - 1 downto 0 do
            let fileSize = files[fileId]
            let spaceId = freeSpaces.Slice(0, fileId).IndexOfAnyExceptInRange(0, fileSize - 1)

            if spaceId >= 0 then
                let slice = blocks.Slice(blocks.LastIndexOf(spaceId))
                let slice = slice.Slice(slice.IndexOf(-1), fileSize)
                freeSpaces[spaceId] <- freeSpaces[spaceId] - fileSize
                blocks.Replace(fileId, -1)
                slice.Fill(fileId)

        let mutable checksum = 0L

        for i in 0 .. blocks.Length - 1 do
            if blocks[i] <> -1 then checksum <- checksum + int64 (i * blocks[i])

        checksum |> Answer.submit
