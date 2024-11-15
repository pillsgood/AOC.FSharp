namespace AOC.FSharp.Common

open System.Numerics
open System.Runtime.CompilerServices

[<Struct>]
type line<'v, 'u when 'v :> IVector<'v, 'u> and 'u :> INumber<'u>> =
    struct
        val mutable a: 'v
        val mutable b: 'v

        new(a: 'v, b: 'v) = { a = a; b = b }

        new(line: line<_, _>, ?a: 'v, ?b: 'v) =
            { a = defaultArg a line.a
              b = defaultArg b line.b }
    end

    member this.ab = this.b - this.a
    member this.ba = this.a - this.b

type line2d<'u & #INumber<'u>> = line<vector2<'u>, 'u>

[<AutoOpen>]
module lineExt =
    let inline length (line: line<_, _>) = line.ab |> magnitude

    let inline sqrLength (line: line<_, 'u & #INumber<'u>>) = line.ab |> sqrMagnitude

    let inline manhattan (line: line<_, 'u & #INumber<'u>>) = line.ab |> manhattan

    let contains (l: line<'v & #IVector<'v, 'u>, 'u & #INumber<'u>>) (point: 'v) =
        let ap = line<'v, 'u> (l.a, point)
        let pb = line<'v, 'u> (point, l.b)
        let apL: float = length ap
        let pbL: float = length pb
        let abL: float = length l
        let diff: float = abL - (apL + pbL)
        abs diff <= epsilon
