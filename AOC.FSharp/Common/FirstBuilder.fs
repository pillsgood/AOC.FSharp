namespace AOC.FSharp.Common

[<AutoOpen>]
module Builders =
    type FirstBuilder() =
        member this.ReturnFrom(x) = x

        member this.Combine(a, b) =
            match a with
            | Some _ -> a // a succeeds -- use it
            | None -> b // a fails -- use b instead

        member this.Delay(f) = f ()

    let first = FirstBuilder()
