namespace AOC.FSharp.Y2024

open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day11 =
    let input: int64 array = Input.fetch |> (String.split " " >> Array.map int64)

    let (|Split|_|) value =
        match (string value) with
        | s when s.Length % 2 = 0 -> s |> String.split2 |> (fun (l, r) -> Some(int64 l, int64 r))
        | _ -> None

    let sim =
        memoizeRec2
        <| fun f' i value ->
            let inline f x = f' (i - 1) x
            let inline step x =
                match x with
                | 0L -> f 1L
                | Split(l, r) -> f l + f r
                | x -> f (2024L * x)
            if i = 0 then 1L else (step value)

    [<Test>]
    let Part1 () = input |> Array.Parallel.sumBy (sim 25) |> Answer.submit

    [<Test>]
    let Part2 () = input |> Array.Parallel.sumBy (sim 75) |> Answer.submit
