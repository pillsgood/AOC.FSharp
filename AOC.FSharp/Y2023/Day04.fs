namespace AOC.FSharp.Y2023

open System.Text.RegularExpressions
open AOC.FSharp.Common
open NUnit.Framework
open Pillsgood.AdventOfCode

type Card = { id: int; count: int }

type Day04() =
    inherit AocFixture()

    let cards =
        let pattern = Regex(@"Card\s*(\d+): ((?:\s*\d+)+) \| ((?:\s*\d+)+)")

        let parse =
            function
            | RegexMatch pattern [ id; pot; jackpot ] ->
                { id = int id
                  count =
                    (pot |> String.split " ")
                    |> Seq.intersect (jackpot |> String.split " ")
                    |> Seq.length }
                |> Some
            | _ -> None

        base.Input.Get<string[]>() |> Seq.choose parse |> Seq.toList

    [<Test>]
    member _.Part1() =
        let eval = fun count -> if count > 0 then 1 <<< count - 1 else 0
        cards |> Seq.sumBy (_.count >> eval) |> base.Answer.Submit

    [<Test>]
    member _.Part2() =
        let count card = min card.count (cards.Length - card.id)

        let update registry (src, dst) =
            registry |> Array.updateAt dst (registry[dst] + registry[src])

        cards
        |> Seq.collect (fun card -> Seq.init (count card) (fun i -> card.id - 1, card.id + i))
        |> Seq.fold update (Array.init cards.Length (fun _ -> 1))
        |> Seq.sum
        |> base.Answer.Submit
