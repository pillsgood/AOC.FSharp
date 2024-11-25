[<AutoOpen>]
module DimensionExt

open AOC.FSharp.Common

type List =
    static member inline dimensions(xs: 'a seq list) =
        let height = List.length xs
        let width = List.head xs |> Seq.length
        int2 (width, height)

    static member inline dimensions(xs: string list) =
        let height = List.length xs
        let width = List.head xs |> _.Length
        int2 (width, height)

type Array =
    static member inline dimensions(xs: 'a seq array) =
        let height = Array.length xs
        let width = Array.head xs |> Seq.length
        int2 (width, height)

    static member inline dimensions(xs: string array) =
        let height = Array.length xs
        let width = Array.head xs |> _.Length
        int2 (width, height)


type Seq =
    static member inline dimensions(xs: 'a seq seq) =
        let height = Seq.length xs
        let width = Seq.head xs |> Seq.length
        int2 (width, height)

    static member inline dimensions(xs: string seq) =
        let height = Seq.length xs
        let width = Seq.head xs |> _.Length
        int2 (width, height)
