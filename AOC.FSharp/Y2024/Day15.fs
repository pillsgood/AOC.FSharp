namespace AOC.FSharp.Y2024

open AOC.FSharp.Common
open Microsoft.FSharp.Core
open NUnit.Framework
open Pillsgood.AdventOfCode

[<AocFixture>]
module Day15 =
    let input =
        Input.fetch<string>
        |> String.split "\n\n"
        |> (fun t -> {| map = t[0] |> String.splitLines; instructions = t[1] |})

    type EntityType =
        | Bot
        | Box
        | Wall

    type Entity = { Type: EntityType; Rect: rectInt }

    let instructions =
        input.instructions
        |> Seq.choose (function
            | '>' -> Some int2.right
            | '<' -> Some int2.left
            | '^' -> Some -int2.up
            | 'v' -> Some -int2.down
            | _ -> None)
        |> Seq.toArray

    let createEntities factory input = input |> Array.map2d (fun i j c -> int2 (i, j), c) |> Array.choose factory

    let update f stack entities =
        stack
        |> List.map (fun e -> entities |> Array.findIndex ((=) e))
        |> List.iter (fun id ->
            let e = &entities[id]
            e <- f e)

    let getMoveStack (entity: Entity) (move: int2) (entities: Entity array) : Entity list =
        let rec scan (visited: Set<Entity>) (current: Entity) =
            current
            |> Result.validate _.Type.IsWall
            |> Result.bind (fun _ ->
                let updatedVisited = visited.Add(current)
                let collision = current.Rect.Move(move)

                entities
                |> Array.filter (fun e -> e.Rect.Intersects(collision) && not (visited.Contains(e)))
                |> Array.fold (fun acc e -> Result.bind (fun v -> scan v e) acc) (Ok updatedVisited))

        match scan Set.empty entity with
        | Ok resultSet -> resultSet |> Set.toList
        | Error _ -> []

    [<Test>]
    let Part1 () =
        let mutable entities =
            input.map
            |> createEntities (fun (position, c) ->
                match c with
                | '#' -> Some { Type = Wall; Rect = rectInt (position, int2.one) }
                | 'O' -> Some { Type = Box; Rect = rectInt (position, int2.one) }
                | '@' -> Some { Type = Bot; Rect = rectInt (position, int2.one) }
                | _ -> None)

        instructions
        |> Array.iter (fun move ->
            let bot = entities |> Array.find _.Type.IsBot
            let stack = entities |> getMoveStack bot move
            (stack, entities) ||> update (fun e -> { e with Rect = e.Rect.Move move }))

        entities
        |> Array.filter _.Type.IsBox
        |> Array.sumBy (_.Rect.min >> (fun v -> v.x + 100 * v.y))
        |> Answer.submit

    [<Test>]
    let Part2 () =
        let upscale input =
            input
            |> Array.map (
                String.collect (function
                    | '#' -> "##"
                    | 'O' -> "[]"
                    | '@' -> "@."
                    | '.' -> ".."
                    | _ -> "")
            )

        let mutable entities =
            upscale input.map
            |> createEntities (fun (position, c) ->
                match c with
                | '#' -> Some { Type = Wall; Rect = rectInt (position, int2.one) }
                | '[' -> Some { Type = Box; Rect = rectInt (position, int2 (2, 1)) }
                | '@' -> Some { Type = Bot; Rect = rectInt (position, int2.one) }
                | _ -> None)

        instructions
        |> Array.iter (fun move ->
            let bot = entities |> Array.find _.Type.IsBot
            let stack = entities |> getMoveStack bot move
            (stack, entities) ||> update (fun e -> { e with Rect = e.Rect.Move move }))

        entities
        |> Array.filter _.Type.IsBox
        |> Array.sumBy (_.Rect.min >> (fun v -> v.x + 100 * v.y))
        |> Answer.submit
