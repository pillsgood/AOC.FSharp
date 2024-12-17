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

    [<RequireQualifiedAccess>]
    type Op =
        | adv of Operand
        | bxl of Operand
        | bst of Operand
        | jnz of Operand
        | bxc
        | out of Operand
        | bdv of Operand
        | cdv of Operand

    let createOp opcode operand =
        let literal x = Const(uint64 x)
        let combo (x: int) = if x >= 0 && x <= 3 then Const(uint64 x) else Reg(enum<Register> (x - 4))

        match opcode with
        | 0 -> Op.adv (combo operand)
        | 1 -> Op.bxl (literal operand)
        | 2 -> Op.bst (combo operand)
        | 3 -> Op.jnz (literal operand)
        | 4 -> Op.bxc
        | 5 -> Op.out (combo operand)
        | 6 -> Op.bdv (combo operand)
        | 7 -> Op.cdv (combo operand)
        | _ -> failwith "Invalid opcode"

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

    let execute (instruction: Op) (ptr: int byref) (buf: Buffer byref) =
        let pow (x: uint64) : uint64 = pown 2uL (int x)

        match instruction with
        | Op.adv operand ->
            let mutable reg = &buf[Register.A]
            reg <- reg / (pow buf[operand])
            ptr <- ptr + 1
        | Op.bxl operand ->
            let mutable reg = &buf[Register.B]
            reg <- reg ^^^ buf[operand]
            ptr <- ptr + 1
        | Op.bst operand ->
            let mutable reg = &buf[Register.B]
            reg <- buf[operand] % 8uL
            ptr <- ptr + 1
        | Op.jnz operand -> if buf[Register.A] = 0uL then ptr <- ptr + 1 else ptr <- int (buf[operand]) / 2
        | Op.bxc ->
            let mutable reg = &buf[Register.B]
            reg <- reg ^^^ buf[Register.C]
            ptr <- ptr + 1
        | Op.out operand ->
            let mutable output = &buf.output
            output <- int (buf[operand] % 8uL) :: output
            ptr <- ptr + 1
        | Op.bdv operand ->
            let mutable reg = &buf[Register.B]
            reg <- buf[Register.A] / (pow buf[operand])
            ptr <- ptr + 1
        | Op.cdv operand ->
            let mutable reg = &buf[Register.C]
            reg <- buf[Register.A] / (pow buf[operand])
            ptr <- ptr + 1

    let run (program: Op list) (buffer: Buffer) =
        let mutable buffer = buffer
        let mutable ptr = 0

        while ptr < program.Length do
            let instruction = program[ptr]
            execute instruction &ptr &buffer

        buffer.output |> List.rev

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
            | [ opcode; operand ] -> Some(createOp opcode operand)
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
