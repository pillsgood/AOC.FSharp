namespace AOC.FSharp.Y2023

open System
open System.Text.RegularExpressions
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day01 =
    let input: string[] = Input.fetch

    [<Test>]
    let Part1 () =
        let getCalibrationValue row =
            let findDigit search = row |> search Char.IsDigit |> string
            let first = findDigit Seq.find
            let last = findDigit Seq.findBack
            first + last |> int

        input |> Seq.sumBy getCalibrationValue |> Answer.submit

    [<Test>]
    let Part2 () =
        let numbers = [| "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]

        let pattern = numbers |> Seq.append (Seq.singleton @"\d") |> String.concat "|"

        let forward = Regex(pattern)
        let backward = Regex(pattern, RegexOptions.RightToLeft)

        let (|Digit|_|) (str: string) = if str.Length = 1 && Char.IsDigit(str[0]) then Some(str[0]) else None

        let getCalibrationValue row =
            let find (rgx: Regex) : char =
                match rgx.Match(row).Value with
                | Digit c -> c
                | word -> numbers |> Seq.findIndex (fun w -> w = word) |> char |> (+) '1'

            let first = find forward |> string
            let last = find backward |> string
            first + last |> int

        input |> Seq.sumBy getCalibrationValue |> Answer.submit
