namespace AOC.FSharp.Y2023

open System.Text.RegularExpressions
open NUnit.Framework
open Pillsgood.AdventOfCode
open AOC.FSharp.Common

[<AocFixture>]
module Day06 =
    type Race = { Time: int64; Distance: int64 }

    let input: string = Input.fetch

    let records =
        let pattern = Regex(@"Time:(?:\s*(?<time>\d+))+\nDistance:(?:\s*(?<distance>\d+))+")

        match input with
        | Match pattern [ time; distance ] ->
            [ for t, d in (time.Captures, distance.Captures) ||> Seq.zip -> { Time = int t.Value; Distance = int d.Value } ]
        | _ -> []

    let eval race =
        let sim race t =
            let d = t * (race.Time - t)
            (d > race.Distance) ?-> (t, t - 1L)

        let v = (race.Time / 2L) |> Seq.unfold (sim race) |> Seq.length
        (int64 v * 2L) + (race.Time % 2L - 1L)

    [<Test>]
    let Part1 () = records |> Seq.map eval |> Seq.reduce (*) |> Answer.submit

    [<Test>]
    let Part2 () =
        let concat f a b = int64 (((f >> string) a) + ((f >> string) b))

        let record =
            records
            |> Seq.reduce (fun a b -> { Time = concat _.Time a b; Distance = concat _.Distance a b })

        eval record |> Answer.submit
