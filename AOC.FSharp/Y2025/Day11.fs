namespace AOC.FSharp.Y2025

open System.Text.RegularExpressions
open AOC.FSharp.Common
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day11 =
    type Connection = { src: string; dsts: string list }

    let input: Map<string, Connection> =
        Input.fetch
        |> Array.choose (function
            | Match (Regex @"^(\w{3})\:(?:\s?(\w{3})\s??)+$") [ src; dsts ] ->
                Some { src = src.Value; dsts = dsts.Captures |> Seq.map _.Value |> List.ofSeq }
            | _ -> None)
        |> Array.map (fun c -> c.src, c)
        |> Map.ofArray

    let countPaths (dst: string) req src =
        memoizeRec
        <| fun f' (req, current) ->
            let req' = req |> List.filter ((<>) current)

            let search node : int64 =
                input
                |> Map.tryFind node
                |> Option.map _.dsts
                |> Option.defaultValue []
                |> List.sumBy (fun dst' -> f' (req', dst'))

            if current = dst then
                if req.IsEmpty then 1L else 0L
            else
                search current
        |> fun f -> f (req, src)

    [<Test>]
    let Part1 () = "you" |> countPaths "out" [] |> Answer.submit

    [<Test>]
    let Part2 () = "svr" |> countPaths "out" [ "fft"; "dac" ] |> Answer.submit
