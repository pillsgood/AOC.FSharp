namespace AOC.FSharp.Y2025

open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day03 =
    let input = Input.fetch |> Array.map String.toArray

    let max n bank =
        let rec f (acc: string) (bank: char array) =
            if acc.Length = n then
                acc
            else
                let c = Array.max bank[.. ^((n - acc.Length) - 1)]
                f $"{acc}{c}" (bank[((bank |> Array.findIndex ((=) c)) + 1) ..])

        f "" bank |> int64

    [<Test>]
    let Part1 () = input |> Array.sumBy (max 2) |> Answer.submit

    [<Test>]
    let Part2 () = input |> Array.sumBy (max 12) |> Answer.submit
