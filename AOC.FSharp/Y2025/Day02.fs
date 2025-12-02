namespace AOC.FSharp.Y2025

open System
open AOC.FSharp.Common
open FSharp.Collections.ParallelSeq
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day02 =
    let input =
        Input.fetch
        |> String.split ","
        |> Array.map (
            (String.split "-")
            >> function
                | [| fst; lst |] -> (int64 fst, int64 lst)
                | _ -> failwith "invalid input"
        )

    [<Test>]
    let Part1 () =
        input
        |> Array.collect (fun (fst, lst) -> [| fst..lst |] |> Array.map string)
        |> PSeq.filter (fun i -> i.Length % 2 = 0 && i |> String.split2 |> (fun (l, r) -> l = r))
        |> PSeq.sumBy int64
        |> Answer.submit

    [<Test>]
    let Part2 () =
        let findDivisors = memoize (fun n -> [| 1 .. n / 2 |] |> Array.filter (fun i -> n % i = 0))

        let isRepeatingPattern (s: string) =
            findDivisors s.Length
            |> Array.exists (fun d ->
                let chunk = s.AsSpan(0, d)
                let mutable repeating = true
                let mutable i = d

                while repeating && i < s.Length do
                    if not (chunk.Equals(s.AsSpan(i, d), StringComparison.Ordinal)) then
                        repeating <- false

                    i <- i + d

                repeating)

        input
        |> Array.collect (fun (fst, lst) -> [| fst..lst |] |> Array.map string)
        |> PSeq.filter isRepeatingPattern
        |> PSeq.sumBy int64
        |> Answer.submit
