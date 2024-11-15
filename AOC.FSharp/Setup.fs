namespace AOC.FSharp

open System
open NUnit.Framework
open Pillsgood.AdventOfCode
open Pillsgood.AdventOfCode.Login

[<SetUpFixture; Parallelizable>]
type Setup() =

    let mutable disposable: IAsyncDisposable option = None

    [<OneTimeSetUp>]
    member _.Start() =
        task {
            let! startup = Registrations.StartAsync(fun cfg -> cfg.WithLogin() |> ignore)
            disposable <- Some startup
        }

    [<OneTimeTearDown>]
    member _.Shutdown() =
        task {
            match disposable with
            | Some d -> do! d.DisposeAsync()
            | None -> ()
        }
