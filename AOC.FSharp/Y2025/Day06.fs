namespace AOC.FSharp.Y2025

open System
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day06 =

    let input =
        let src = Input.fetch<string array>

        let splitIndices =
            src[^0]
            |> Seq.indexed
            |> Seq.choose (fun (i, c) -> if Char.IsWhiteSpace c then None else Some i)
            |> Set.ofSeq

        let split str =
            str
            |> Seq.indexed
            |> Seq.splitBy (fun (i, _) -> splitIndices.Contains(i))
            |> Seq.map ((Seq.map snd) >> Array.ofSeq >> String)

        {| Operands = src[..^1] |> Array.map split |> Seq.transpose
           Operations =
            src[^0]
            |> String.split " "
            |> Array.map (function
                | "*" -> (*)
                | "+" -> (+)
                | op -> failwith $"unknown operation {op}") |}

    [<Test>]
    let Part1 () =
        input.Operands
        |> Seq.map (Seq.map int64)
        |> Seq.zip input.Operations
        |> Seq.sumBy (fun (op, arr) -> arr |> Seq.reduce op)
        |> Answer.submit

    [<Test>]
    let Part2 () =
        let transpose arr =
            arr
            |> Seq.map String.toArray
            |> Array.transpose
            |> Array.choose (fun arr ->
                match String arr |> _.Trim() with
                | "" -> None
                | s -> Some(int64 s))

        input.Operands
        |> Seq.map transpose
        |> Seq.zip input.Operations
        |> Seq.sumBy (fun (op, arr) -> arr |> Array.reduce op)
        |> Answer.submit
