namespace AOC.FSharp.Y2023

open System.Numerics
open NUnit.Framework
open Pillsgood.AdventOfCode
open AOC.FSharp.Common



[<AocFixture>]
module Day07 =

    type HandType =
        | HighCard = 0
        | Pair = 1
        | TwoPair = 2
        | Three = 3
        | FullHouse = 4
        | Four = 5
        | Five = 6

    type Hand =
        { CardVector: BitVector64
          CardSet: BitVector64
          Order: int
          Jokers: int
          Bid: int }

    let getRank =
        function
        | 'T' -> 8
        | 'J' -> 9
        | 'Q' -> 10
        | 'K' -> 11
        | 'A' -> 12
        | c -> int (c - '2')

    let hands =
        let createVector (v: BitVector64) card =
            let rank = getRank card
            v |> BitVector64.updateAt _.Section(0xFu, rank) (fun s -> s <<< 1 ||| 1uL)

        let createSet (v: BitVector64) card =
            let rank = getRank card
            v |> BitVector64.updateAt _.Section(0x1u, rank) (fun _ -> 1uL)

        let createOrder (v: int) card = v <<< 4 ||| (getRank card)

        let parse str =
            match str |> (String.split " ") with
            | [| cards; bid |] ->
                let initial =
                    { CardVector = BitVector64 0uL
                      CardSet = BitVector64 0uL
                      Order = 0
                      Jokers = 0
                      Bid = int bid }

                (initial, cards)
                ||> Seq.fold (fun hand card ->
                    { hand with
                        CardVector = createVector hand.CardVector card
                        CardSet = createSet hand.CardSet card
                        Order = createOrder hand.Order card })
                |> Some
            | _ -> None

        Input.fetch |> Seq.choose parse |> Seq.toList

    let getType hand : HandType =
        let id = int (hand.CardVector.Data % 0xFuL)
        let popCount = BitOperations.PopCount hand.CardSet.Data

        match (id, popCount) with
        | _, 1 -> HandType.Five
        | 1, _ -> HandType.Four
        | 10, _ -> HandType.FullHouse
        | 9, _ -> HandType.Three
        | 7, _ -> HandType.TwoPair
        | 6, _ -> HandType.Pair
        | _ -> HandType.HighCard

    let getJokerType hand : HandType =
        let id = int (hand.CardVector.Data % 0xFuL)
        let jokers = hand.Jokers

        match (id, jokers) with
        | 0, 1 -> HandType.Five // v: 0b1111
        | 7, 2 -> HandType.Five // v: 0b0111
        | 3, 3 -> HandType.Five // v: 0b0011
        | 1, 4 -> HandType.Five // v: 0b0001
        | _, 5 -> HandType.Five // v: 0b0000
        | 8, 1 -> HandType.Four // v: 0b0111 + 0b1
        | 4, 2 -> HandType.Four // v: 0b0011 + 0b1
        | 2, 3 -> HandType.Four // v: 0b0001 + 0b1
        | 6, 1 -> HandType.FullHouse // v: 0b011 + 0b11
        | 5, 1 -> HandType.Three // v: 0b011 + 2
        | 3, 2 -> HandType.Three // v: 0b001 + 2
        | 4, 1 -> HandType.Pair // v: 0b01 + 3
        | _ -> getType hand

    [<Test>]
    let Part1 () =
        hands
        |> Seq.sortBy (fun h -> (getType h, h.Order))
        |> Seq.mapi (fun i h -> (i + 1) * h.Bid)
        |> Seq.sum
        |> Answer.submit

    [<Test>]
    let Part2 () =
        let reinterpretJokers hand =
            let rank = getRank 'J'
            let trim section v = v |> BitVector64.updateAt section (fun _ -> 0uL)
            let split (v: int) = Seq.init 5 (fun i -> (v &&& (0xF <<< (i * 4)) >>> (i * 4)))

            let reorder v =
                split v
                |> Seq.map (fun i -> if i = rank then 0 else (i + 1))
                |> Seq.reduceBack (fun a b -> b <<< 4 ||| a)

            let countJokers v = split v |> Seq.sumBy (fun i -> if i = rank then 1 else 0)

            { hand with
                CardVector = hand.CardVector |> trim _.Section(0xFu, rank)
                CardSet = hand.CardSet |> trim _.Section(0x1u, rank)
                Order = reorder hand.Order
                Jokers = countJokers hand.Order }

        hands
        |> Seq.map reinterpretJokers
        |> Seq.sortBy (fun h -> (getJokerType h, h.Order))
        |> Seq.mapi (fun i h -> (i + 1) * h.Bid)
        |> Seq.sum
        |> Answer.submit
