namespace AOC.FSharp

open System
open System.IO
open NUnit.Framework
open Pillsgood.AdventOfCode
open Pillsgood.AdventOfCode.Login

[<SetUpFixture; Parallelizable(ParallelScope.Fixtures)>]
type Setup() =

    let mutable disposable: IDisposable option = None

    [<OneTimeSetUp>]
    member _.Start() =
        let cache =
            let dir =
                Path.Combine(
                    Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData),
                    "pillsgood.adventofcode"
                )

            Directory.CreateDirectory(dir) |> ignore
            Path.Combine(dir, "store.db")

        let startup = Aoc.Start(fun cfg -> cfg.WithLogin().WithCachePath(cache) |> ignore)

        disposable <- Some(startup)

    [<OneTimeTearDown>]
    member _.Shutdown() =
        match disposable with
        | Some d -> d.Dispose()
        | None -> ()
