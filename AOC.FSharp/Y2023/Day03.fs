namespace AOC.FSharp.Y2023

open System
open System.Text.RegularExpressions
open AOC.FSharp.Common
open NUnit.Framework
open Pillsgood.AdventOfCode

[<TestFixture>]
type Day03() =
    inherit AocFixture()

    let mutable symbols: (int2 * char) list = []
    let mutable parts: (rectInt * int) list = []

    [<OneTimeSetUp>]
    member _.Setup() =
        let input = base.Input.Get<string[]>()
        parts <-
            [ for y in 0 .. input.Length - 1 ->
                  match input[y] with
                  | Matches (Regex(@"\d+")) parts ->
                      parts
                      |> List.map (fun part -> (rectInt (int2 (part.Index, y), vector2 (part.Length - 1, 0)), int part.Value))
                  | _ -> [] ]
            |> List.concat

        symbols <-
            [ for y in 0 .. input.Length - 1 do
                  for x in 0 .. input[y].Length - 1 do
                      let c = input[y].[x]

                      if not (Char.IsDigit c || c = '.') then
                          yield (int2 (x, y), c) ]

    [<Test>]
    member _.Part1() =
        let test (rect: rectInt) =
            let area = rect |> grow int2.one
            symbols |> Seq.exists (fst >> fun p -> area |> contains p)

        parts |> Seq.where (fst >> test) |> Seq.sumBy snd |> base.Answer.Submit

    [<Test>]
    member _.Part2() =
        let getGearRatio point =
            let area = rect (point - int2.one, int2 (2, 2))

            match parts |> List.where (fst >> intersects area) |> List.map snd with
            | [ fst; snd ] -> Some(fst * snd)
            | _ -> None

        symbols
        |> Seq.where (snd >> fun c -> c = '*')
        |> Seq.choose (fst >> getGearRatio)
        |> Seq.sum
        |> base.Answer.Submit
