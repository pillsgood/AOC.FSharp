namespace AOC.FSharp.Y2023

open System.Text.RegularExpressions
open FSUnit
open Microsoft.FSharp.Core
open NUnit.Framework
open AOC.FSharp.Common
open Pillsgood.AdventOfCode


[<AocFixture>]
module Day10 =
    type Pipe =
        | NS
        | WE
        | NE
        | NW
        | SW
        | SE

        member this.connections =
            match this with
            | NS -> (int2.up, int2.down)
            | WE -> (int2.left, int2.right)
            | NE -> (int2.up, int2.right)
            | NW -> (int2.up, int2.left)
            | SW -> (int2.down, int2.left)
            | SE -> (int2.down, int2.right)

    type Tile =
        | Pipe of position: int2 * pipe: Pipe
        | Ground of position: int2
        | Start of position: int2

        member this.position =
            match this with
            | Pipe(pos, _) -> pos
            | Ground(pos) -> pos
            | Start(pos) -> pos

    let input: string[] = Input.fetch

    let width, height = input[0].Length, input.Length
    let getPosition i = int2 (i % width, (height - 1) - i / width)

    let map =
        let parse position =
            function
            | '|' -> Some(Tile.Pipe(position, NS))
            | '-' -> Some(Tile.Pipe(position, WE))
            | 'L' -> Some(Tile.Pipe(position, NE))
            | 'J' -> Some(Tile.Pipe(position, NW))
            | '7' -> Some(Tile.Pipe(position, SW))
            | 'F' -> Some(Tile.Pipe(position, SE))
            | '.' -> Some(Tile.Ground(position))
            | 'S' -> Some(Tile.Start(position))
            | _ -> None

        input
        |> Seq.collect id
        |> Seq.indexed
        |> Seq.choose (fun (i, c) -> parse (getPosition i) c)
        |> Seq.addKey _.position

    let getNeighbors (current: Tile) =
        int2.cardinalDirections
        |> Seq.map (fun v -> map |> Map.tryFind (current.position + v))
        |> Seq.choose id

    let canEnter (tile: Tile) (current: int2) =
        match tile with
        | Tile.Ground _ -> false
        | Tile.Start(position) -> vector.sqrDistance position current = 1
        | Tile.Pipe(position, pipe) ->
            let move = position - current
            pipe.connections |> fun (a, b) -> move = -a || move = -b

    let getConnection (tile: Tile) (current: Tile) : int2 option =
        let move = tile.position - current.position

        match tile with
        | Tile.Pipe(_, pipe) when current.position |> canEnter tile ->
            match pipe.connections with
            | a, b when -a = move -> Some b
            | a, b when -b = move -> Some a
            | _ -> None
        | _ -> None

    let getStartConnection current =
        getNeighbors current
        |> Seq.tryFind (flip canEnter current.position)
        |> Option.map (fun exit -> exit.position - current.position)

    let traverse (current: Tile) (previous: Tile option) =
        let next =
            match current, previous with
            | Tile.Start _, None -> getStartConnection current
            | _, Some previous -> getConnection current previous
            | _ -> None
            |> Option.map (fun exit -> map |> Map.find (current.position + exit))

        next
        |> Option.bind (fun next ->
            match next with
            | Pipe _
            | Start _ -> Some(current, next)
            | _ -> None)

    let origin = map |> Map.findValue _.IsStart

    [<Test>]
    let Part1 () =
        origin
        |> Seq.scanUnfold traverse
        |> Seq.length
        |> (fun i -> i / 2)
        |> Answer.submit


    [<Test>]
    let Part2 () =
        let path = origin |> Seq.scanUnfold traverse

        let mutable input = input |> Array.copy

        map
        |> Map.keys
        |> Seq.except (path |> Seq.map _.position)
        |> Seq.iter (fun p ->
            let x, y = p.x, input.Length - (p.y + 1)
            let mutable str: char array = input[y] |> Seq.toArray
            str[x] <- '.'
            input[y] <- new string (str))

        let p1 = Regex("F-*7|L-*J")
        let p2 = Regex("F-*J|L-*7")

        let input =
            input
            |> Seq.map (fun str -> p1.Replace(str, "O"))
            |> Seq.map (fun str -> p2.Replace(str, "|"))
            |> Seq.toList

        input
        |> Seq.sumBy (fun str ->
            ((0, 0), str)
            ||> Seq.fold (fun (parity, acc) c ->
                match c with
                | '.' when parity % 2 = 1 -> parity, acc + 1
                | '|' -> parity + 1, acc
                | _ -> parity, acc)
            |> snd)
        |> Answer.submit
