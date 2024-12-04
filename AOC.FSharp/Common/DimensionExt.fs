[<AutoOpen>]
module DimensionExt

open AOC.FSharp.Common

type List =
    static member inline dimensions<'a when 'a: (member Length: int)>(xs: 'a list) =
        let height = List.length xs
        let width = List.head xs |> _.Length
        int2 (width, height)

type Array =
    static member inline dimensions<'a when 'a: (member Length: int)>(xs: 'a array) =
        let height = Array.length xs
        let width = Array.head xs |> _.Length
        int2 (width, height)

type Seq =
    static member inline dimensions<'a when 'a: (member Length: int)>(xs: 'a seq) =
        let height = Seq.length xs
        let width = Seq.head xs |> _.Length
        int2 (width, height)
