namespace AOC.FSharp.Y2024

open System.Collections.Concurrent
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day11 =
    let input: int64 array = Input.fetch |> (String.split " " >> Array.map int64)

    let (|IsSplit|_|) value =
        string value
        |> fun s ->
            match s.Length with
            | l when l % 2 = 0 -> Some(int64 s[.. (l / 2) - 1], int64 s[l / 2 ..])
            | _ -> None

    let cache = ConcurrentDictionary<int64 * int, int64>()

    let rec blink i value =
        let inline f x = cache.GetOrAdd((x, i), fun (x, i) -> blink (i - 1) x)

        let inline convert x =
            match x with
            | 0L -> f 1L
            | IsSplit(l, r) -> f l + f r
            | x -> f (2024L * x)

        if i = 0 then 1L else (convert value)


    [<Test>]
    let Part1 () = input |> Array.Parallel.sumBy (blink 25) |> Answer.submit

    [<Test>]
    let Part2 () = input |> Array.Parallel.sumBy (blink 75) |> Answer.submit
