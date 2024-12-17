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

    type Operand =
        | Const of uint64
        | Reg of Register

    type Opcode =
        | adv = 0
        | bxl = 1
        | bst = 2
        | jnz = 3
        | bxc = 4
        | out = 5
        | bdv = 6
        | cdv = 7

    type Buffer =
        struct
            val mutable output: int list
            val mutable registers: uint64[]
            new(registers: uint64[]) = { registers = registers; output = [] }
        end

        member this.Item
            with get (reg: Register): uint64 byref = &this.registers[int reg]

        member this.Item
            with get (operand: Operand): uint64 =
                match operand with
                | Const c -> c
                | Reg r -> this[r]

    let execute (opcode: Opcode, operand: Operand) (ptr: int byref) (buf: Buffer byref) =
        let mutable output = &buf.output
        let pow (x: uint64) : uint64 = pown 2uL (int x)

        match opcode with
        | Opcode.adv ->
            let mutable reg = &buf[Register.A]
            reg <- reg / (pow buf[operand])
            ptr <- ptr + 1
        | Opcode.bxl ->
            let mutable reg = &buf[Register.B]
            reg <- reg ^^^ buf[operand]
            ptr <- ptr + 1
        | Opcode.bst ->
            let mutable reg = &buf[Register.B]
            reg <- buf[operand] % 8uL
            ptr <- ptr + 1
        | Opcode.jnz -> if buf[Register.A] <> 0uL then ptr <- int (buf[operand]) / 2 else ptr <- ptr + 1
        | Opcode.bxc ->
            let mutable reg = &buf[Register.B]
            reg <- reg ^^^ buf[Register.C]
            ptr <- ptr + 1
        | Opcode.out ->
            output <- int (buf[operand] % 8uL) :: output
            ptr <- ptr + 1
        | Opcode.bdv ->
            let mutable reg = &buf[Register.B]
            reg <- buf[Register.A] / (pow buf[operand])
            ptr <- ptr + 1
        | Opcode.cdv ->
            let mutable reg = &buf[Register.C]
            reg <- buf[Register.A] / (pow buf[operand])
            ptr <- ptr + 1
        | _ -> ()

    let run (program: (Opcode * Operand) list) (computer: Buffer) =
        let mutable computer = computer
        let mutable ptr = 0

        while ptr < program.Length do
            let instruction = program[ptr]
            execute instruction &ptr &computer

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
           | Opcode.adv
           | Opcode.bst
           | Opcode.out
           | Opcode.bdv
           | Opcode.cdv -> combo
           | Opcode.bxl
           | Opcode.jnz
           | Opcode.bxc -> literal
           | _ -> failwithf $"Unknown opcode: %A{opcode}"

    let registers, input =
        let pattern =
            Regex(
                @"Register A: (\d+)\s*Register B: (\d+)\s*Register C: (\d+)\s*Program: ((?:\d+,?)+)",
                RegexOptions.Singleline
            )

        match Input.fetch<string> with
        | MatchValue pattern [ a; b; c; p ] ->
            [| uint64 a; uint64 b; uint64 c |], p |> String.split "," |> Array.map int |> List.ofArray
        | _ -> failwith "Invalid input format"

    let program =
        input
        |> List.chunkBySize 2
        |> List.choose (function
            | [ opcode; operand ] -> Some(parseInstruction opcode operand)
            | _ -> None)

    [<Test>]
    let Part1 () =
        let buffer = Buffer(registers)
        buffer |> run program |> List.map string |> String.concat "," |> Answer.submit

    [<Test>]
    let Part2 () =
        let run a = Buffer([| a; 0uL; 0uL |]) |> run program

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
