namespace AOC.FSharp.Y2023

open NUnit.Framework
open Pillsgood.AdventOfCode
open AOC.FSharp.Common

type Pipe =
    | NS
    | WE
    | NE
    | NW
    | SW
    | SE

    member this.connections: int2 * int2 =
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

[<TestFixture>]
type Day10() =
    inherit AocFixture()

    let input =
        """
        ..........
        .S------7.
        .|F----7|.
        .||....||.
        .||....||.
        .|L-7F-J|.
        .|..||..|.
        .L--JL--J.
        ..........
        """
        |> String.splitLines

    let input = base.Input.Get<string[]>()
    let (width, height) = (input |> Seq.head |> Seq.length, input.Length)

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

        let getVector i = int2 (i % width, height - i / width)

        input
        |> Seq.collect id
        |> Seq.indexed
        |> Seq.choose (fun (i, c) -> parse (getVector i) c)
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


    [<Test>]
    member _.Part1() =
        let rec traverse acc current previous =
            let next =
                getConnection current previous
                |> Option.map (fun exit -> map |> Map.find (current.position + exit))

            match next with
            | Some nextTile -> traverse (acc + 1) nextTile current
            | None -> acc


        let origin = map |> Map.findValue _.IsStart
        let tile = getNeighbors origin |> Seq.find (flip canEnter origin.position)

        (tile, origin) ||> traverse 0 |> (fun i -> (i + 1) / 2) |> base.Answer.Submit

    [<Test>]
    member _.Part2() = ignore
