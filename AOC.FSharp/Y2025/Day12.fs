namespace AOC.FSharp.Y2025

open System
open System.Text.RegularExpressions
open AOC.FSharp.Common
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day12 =
    type Shape = { Id: int; Points: int2 array }

    type Spec = { Bounds: rectInt; Requirements: (int * int) List }

    let input =
        let segments =
            Input.fetch<string array>
            |> Seq.splitBy String.IsNullOrWhiteSpace
            |> Seq.map (Array.filter (not << String.IsNullOrWhiteSpace))
            |> Array.ofSeq

        {| shapes =
            segments[..^1]
            |> Array.map Array.tail
            |> Array.mapi (fun i s ->
                { Id = i; Points = s |> Array.choose2d (fun i j c -> if c = '#' then Some(int2 (i, j)) else None) })
            |> Map.create _.Id
           specs =
            segments[^0]
            |> Array.choose (function
                | Match (Regex @"^(\d+)x(\d+)\: (?:(\d+)\s?)+$") [ w; h; req's ] ->
                    Some
                        { Bounds = rectInt (int2.zero, int2 (int w.Value, int h.Value))
                          Requirements = req's.Captures |> Seq.map (_.Value >> int) |> Seq.indexed |> List.ofSeq }
                | _ -> None) |}

    let guaranteed spec =
        let minArea = spec.Requirements |> Seq.sumBy snd
        let w = float spec.Bounds.width / 3.0
        let h = float spec.Bounds.height / 3.0
        (float minArea) <= w * h

    [<Test>]
    let Part1 () = input.specs |> Seq.count guaranteed |> Answer.submit
