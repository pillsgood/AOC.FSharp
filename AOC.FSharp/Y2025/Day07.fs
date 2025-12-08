namespace AOC.FSharp.Y2025

open AOC.FSharp.Common
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day07 =
    let input =
        Input.fetch<string array>
        |> fun src ->
            {| Splitters =
                src
                |> Array.choose2d (fun i j c -> if c = '^' then Some(int2 (i, j)) else None)
                |> Set.ofArray
               Start =
                src
                |> Array.choose2d (fun i j c -> if c = 'S' then Some(int2 (i, j)) else None)
                |> Array.head
               Length = src.Length |}

    [<Test>]
    let Part1 () =
        let rec scan (t: int) (acc: int list) (j: int) =
            if j >= input.Length then
                t
            else
                let acc', t' =
                    acc
                    |> List.fold
                        (fun (next, counts) i ->
                            match input.Splitters with
                            | Contains(int2 (i, j)) -> (i - 1) :: (i + 1) :: next, counts + 1
                            | _ -> i :: next, counts)
                        ([], 0)

                scan (t + t') (List.distinct acc') (j + 2)

        scan 0 [ input.Start.x ] input.Start.y |> Answer.submit

    [<Test>]
    let Part2 () =
        let rec scan (acc: (int * int64) list) (j: int) =
            if j >= input.Length then
                acc |> List.sumBy snd
            else
                let acc' =
                    acc
                    |> List.collect (fun (i, t) ->
                        match input.Splitters with
                        | Contains(int2 (i, j)) -> [ i - 1, t; i + 1, t ]
                        | _ -> [ i, t ])
                    |> List.groupBy fst
                    |> List.map (fun (i, t) -> i, t |> List.sumBy snd)

                scan acc' (j + 2)

        scan [ input.Start.x, 1L ] input.Start.y |> Answer.submit
