namespace AOC.FSharp.Y2024

open System.Text.RegularExpressions
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day17 =
    type Register =
        | A = 0
        | B = 1
        | C = 2

    type Opcode =
        | adv = 0
        | bxl = 1
        | bst = 2
        | jnz = 3
        | bxc = 4
        | out = 5
        | bdv = 6
        | cdv = 7

    type Operand =
        | Const of uint64
        | Reg of Register

    type Computer =
        struct
            val mutable ptr: int
            val mutable output: int list
            val mutable registers: uint64[]

            new(registers: uint64[]) =
                { registers = registers
                  ptr = 0
                  output = [] }
        end

        member this.Item
            with get (reg: Register): uint64 byref = &this.registers[int reg]

        member this.Item
            with get (operand: Operand): uint64 =
                match operand with
                | Const c -> c
                | Reg r -> this[r]

    let execute (opcode: Opcode, operand: Operand) (comp: Computer byref) =
        let mutable ptr = &comp.ptr
        let mutable output = &comp.output
        let pow (x: uint64) : uint64 = pown 2uL (int x)

        match opcode with
        | Opcode.adv ->
            let mutable reg = &comp[Register.A]
            reg <- reg / (pow comp[operand])
            ptr <- ptr + 2
        | Opcode.bxl ->
            let mutable reg = &comp[Register.B]
            reg <- reg ^^^ comp[operand]
            ptr <- ptr + 2
        | Opcode.bst ->
            let mutable reg = &comp[Register.B]
            reg <- comp[operand] % 8uL
            ptr <- ptr + 2
        | Opcode.jnz -> if comp[Register.A] <> 0uL then ptr <- int (comp[operand]) else ptr <- ptr + 2
        | Opcode.bxc ->
            let mutable reg = &comp[Register.B]
            reg <- reg ^^^ comp[Register.C]
            ptr <- ptr + 2
        | Opcode.out ->
            output <- int (comp[operand] % 8uL) :: output
            ptr <- ptr + 2
        | Opcode.bdv ->
            let mutable reg = &comp[Register.B]
            reg <- comp[Register.A] / (pow comp[operand])
            ptr <- ptr + 2
        | Opcode.cdv ->
            let mutable reg = &comp[Register.C]
            reg <- comp[Register.A] / (pow comp[operand])
            ptr <- ptr + 2
        | _ -> ()

    let run (program: (Opcode * Operand) list) (computer: Computer) =
        let mutable computer = computer

        while (computer.ptr / 2) < program.Length do
            let instruction = program[computer.ptr / 2]
            execute instruction &computer

        computer.output |> List.rev

    let parseInstruction opcode operand =
        let opcode = enum<Opcode> opcode
        let literal x = Operand.Const(uint64 x)

        let combo (x: int) =
            if x >= 0 && x <= 3 then
                Operand.Const(uint64 x)
            else
                Operand.Reg(enum<Register> (x - 4))

        opcode,
        operand
        |> match opcode with
           | Opcode.adv -> combo
           | Opcode.bxl -> literal
           | Opcode.bst -> combo
           | Opcode.jnz -> literal
           | Opcode.bxc -> literal
           | Opcode.out -> combo
           | Opcode.bdv -> combo
           | Opcode.cdv -> combo
           | _ -> failwithf $"Unknown opcode: %A{opcode}"

    let computer, input =
        let pattern =
            Regex(
                @"Register A: (\d+)\s*Register B: (\d+)\s*Register C: (\d+)\s*Program: ((?:\d+,?)+)",
                RegexOptions.Singleline
            )

        match Input.fetch<string> with
        | MatchValue pattern [ a; b; c; p ] ->
            Computer([| uint64 a; uint64 b; uint64 c |]), p |> String.split "," |> Array.map int |> List.ofArray
        | _ -> failwith "Invalid input format"

    let program =
        input
        |> List.chunkBySize 2
        |> List.choose (function
            | [ opcode; operand ] -> Some(parseInstruction opcode operand)
            | _ -> None)

    [<Test>]
    let Part1 () =
        computer |> run program |> List.map string |> String.concat "," |> Answer.submit

    [<Test>]
    let Part2 () =
        let run a = Computer([| a; 0uL; 0uL |]) |> run program

        let decompile (program: int list) =
            let rec search len current =
                if len = program.Length then
                    [ current ]
                else
                    let outs =
                        List.init 8 (fun i -> (current <<< 3) ||| (uint64 i))
                        |> List.map (fun i -> i, run i)
                        |> List.filter (fun (_, out) -> out[0] = input[^len])

                    outs |> Seq.collect (fun (i, _) -> search (len + 1) i) |> Seq.toList

            search 0 0uL

        decompile input |> Seq.head |> Answer.submit
