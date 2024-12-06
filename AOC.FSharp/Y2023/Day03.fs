namespace AOC.FSharp.Y2023

open System
open System.Text.RegularExpressions
open AOC.FSharp.Common
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day03 =

    open Rect
    let input: string[] = Input.fetch

    let symbols: (int2 * char) list =
        [ for y in 0 .. input.Length - 1 do
              for x in 0 .. input[y].Length - 1 do
                  let c = input[y].[x]
                  if not (Char.IsDigit c || c = '.') then yield (int2 (x, y), c) ]

    let parts: (rectInt * int) list =
        [ for y in 0 .. input.Length - 1 ->
              match input[y] with
              | Matches (Regex(@"\d+")) parts ->
                  parts
                  |> List.map (fun part -> (rectInt (int2 (part.Index, y), vector2 (part.Length - 1, 0)), int part.Value))
              | _ -> [] ]
        |> List.concat

    [<Test>]
    let Part1 () =
        let test (rect: rectInt) =
            let area = rect |> grow int2.one
            symbols |> Seq.exists (fst >> fun p -> area |> contains p)

        parts |> Seq.where (fst >> test) |> Seq.sumBy snd |> Answer.submit

    [<Test>]
    let Part2 () =
        let getGearRatio point =
            let area = rect (point - int2.one, int2 (2, 2))

            match parts |> List.where (fst >> intersects area) |> List.map snd with
            | [ fst; snd ] -> Some(fst * snd)
            | _ -> None

        symbols
        |> Seq.where (snd >> fun c -> c = '*')
        |> Seq.choose (fst >> getGearRatio)
        |> Seq.sum
        |> Answer.submit
