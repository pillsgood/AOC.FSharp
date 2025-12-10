namespace AOC.FSharp.Y2025

open System
open System.Globalization
open System.Text.RegularExpressions
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day10 =
    type Scheme =
        { p1: uint
          p2: int array
          btns: int array array }

    let input =
        let parseLightDiagram =
            String.rev
            >> _.Replace('#', '1').Replace('.', '0')
            >> fun s -> UInt32.Parse(s, NumberStyles.BinaryNumber)

        let parseJoltageReq = String.split "," >> Array.map int

        let parseButton = String.split "," >> Array.map int

        Input.fetch
        |> Array.choose (function
            | Match (Regex @"^\[([.#]+)\]\s*(?:\(([\d,]+)\)\s*)*\{([\d,]+)\}$") [ p1; buttons; p2 ] ->
                Some
                    { p1 = parseLightDiagram p1.Value
                      p2 = parseJoltageReq p2.Value
                      btns = buttons.Captures |> Seq.map (_.Value >> parseButton) |> Array.ofSeq }
            | _ -> None)

    [<Test>]
    let Part1 () =
        let inline search (source: 'a array) =
            Seq.init source.Length (fun i -> i + 1)
            |> Seq.collect (fun n -> source |> Seq.combinations n)

        input
        |> Array.sumBy (fun scheme ->
            scheme.btns
            |> Array.map (Array.fold (fun acc i -> acc ||| (1u <<< i)) 0u)
            |> search
            |> Seq.find (fun btn -> (btn |> List.reduce (fun a b -> a ^^^ b)) = scheme.p1)
            |> _.Length)
        |> Answer.submit

    module private Constraint =
        open Microsoft.Z3

        let solve (buttons: int array array) (dst: int array) =
            use ctx = new Context()
            use solver = ctx.MkOptimize()

            let vars: ArithExpr array = Array.init buttons.Length (fun i -> ctx.MkIntConst($"x{i}"))
            vars |> Array.iter (fun v -> solver.Assert(ctx.MkGe(v, ctx.MkInt(0))))

            dst
            |> Array.iteri (fun dstIdx targetValue ->
                let sumExpr =
                    buttons
                    |> Array.indexed
                    |> Array.choose (fun (idx, button) ->
                        if button |> Array.contains dstIdx then Some vars[idx] else None)
                    |> ctx.MkAdd

                ctx.MkEq(sumExpr, ctx.MkInt(targetValue)) |> solver.Add)

            let objective = ctx.MkAdd(vars) |> solver.MkMinimize

            match solver.Check() with
            | Status.SATISFIABLE -> string objective.Value |> int64
            | _ -> failwith "No solution found"

    [<Test>]
    let Part2 () =
        input
        |> Seq.sumBy (fun scheme -> Constraint.solve scheme.btns scheme.p2)
        |> Answer.submit
