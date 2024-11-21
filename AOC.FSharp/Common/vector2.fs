namespace AOC.FSharp.Common

open System.Numerics
open Microsoft.FSharp.Core

[<Struct>]
type vector2<'u & #INumber<'u>> =
    struct
        val mutable x: 'u
        val mutable y: 'u

        new(x: 'u, y: 'u) = { x = x; y = y }

        new(v: vector2<'u>, ?x: 'u, ?y: 'u) = { x = defaultArg x v.x; y = defaultArg y v.y }
    end

    static member (+)(left: vector2<'u>, right: vector2<'u>) : vector2<'u> = vector2 (left.x + right.x, left.y + right.y)

    static member (-)(left: vector2<'u>, right: vector2<'u>) : vector2<'u> = vector2 (left.x - right.x, left.y - right.y)

    static member (*)(left: vector2<'u>, right: vector2<'u>) : vector2<'u> = vector2 (left.x * right.x, left.y * right.y)

    static member (/)(left: vector2<'u>, right: vector2<'u>) : vector2<'u> = vector2 (left.x / right.x, left.y / right.y)

    static member (~+)(value: vector2<_>) = value
    static member (~-)(value: vector2<_>) = vector2 (-value.x, -value.y)

    static member (*)(left: vector2<'u>, right: 'u) : vector2<'u> = vector2 (left.x * right, left.y * right)

    static member (/)(left: vector2<'u>, right: 'u) : vector2<'u> = vector2 (left.x / right, left.y / right)

    member this.Item
        with get index =
            match index with
            | 0 -> this.x
            | 1 -> this.y
            | _ -> invalidArg "index" "Index out of range"
        and set index value =
            match index with
            | 0 -> this.x <- value
            | 1 -> this.y <- value
            | _ -> invalidArg "index" "Index out of range"

    static member one = vector2 ('u.One, 'u.One)
    static member zero = vector2 ('u.Zero, 'u.Zero)
    static member AdditiveIdentity = vector2 ('u.Zero, 'u.Zero)
    static member MultiplicativeIdentity = vector2 ('u.One, 'u.One)
    static member size = 2

    override this.ToString() = $"({this.x}, {this.y})"

    interface IVector<vector2<'u>, 'u> with
        static member (+)(left: vector2<'u>, right: vector2<'u>) : vector2<'u> = left + right
        static member (-)(left: vector2<'u>, right: vector2<'u>) : vector2<'u> = left - right
        static member (*)(left: vector2<'u>, right: vector2<'u>) : vector2<'u> = left * right
        static member (/)(left: vector2<'u>, right: vector2<'u>) : vector2<'u> = left / right
        static member (~+)(value: vector2<_>) = value
        static member (~-)(value: vector2<_>) = -value
        static member one = vector2.one
        static member zero = vector2.zero
        static member AdditiveIdentity = vector2.AdditiveIdentity
        static member MultiplicativeIdentity = vector2.MultiplicativeIdentity
        static member size = vector2<'u>.size

        member this.Item
            with get index = this[index]
            and set index value = this[index] <- value

type int2 = vector2<int>
type float2 = vector2<float32>

module int2 =
    let up = int2 (0, 1)
    let right = int2 (1, 0)
    let down = int2 (0, -1)
    let left = int2 (-1, 0)
    let cardinalDirections = [| up; right; down; left |]
