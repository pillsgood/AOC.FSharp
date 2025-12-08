namespace AOC.FSharp.Common

open System.Numerics
open Microsoft.FSharp.Core
open Vector

[<Struct>]
type vector3<'u & #INumber<'u>> =
    struct
        val mutable x: 'u
        val mutable y: 'u
        val mutable z: 'u

        new(x: 'u, y: 'u, z: 'u) =
            { x = x
              y = y
              z = z }

        // new(v: vector3<'u>, ?x: 'u, ?y: 'u, ?z: 'u) =
        //     { x = defaultArg x v.x
        //       y = defaultArg y v.y
        //       z = defaultArg z v.z }
    end

    static member (+)(left: vector3<'u>, right: vector3<'u>) : vector3<'u> =
        vector3 (left.x + right.x, left.y + right.y, left.z + right.z)

    static member (-)(left: vector3<'u>, right: vector3<'u>) : vector3<'u> =
        vector3 (left.x - right.x, left.y - right.y, left.z - right.z)

    static member (*)(left: vector3<'u>, right: vector3<'u>) : vector3<'u> =
        vector3 (left.x * right.x, left.y * right.y, left.z * right.z)

    static member (/)(left: vector3<'u>, right: vector3<'u>) : vector3<'u> =
        vector3 (left.x / right.x, left.y / right.y, left.z / right.z)

    static member (~+)(value: vector3<_>) = value
    static member (~-)(value: vector3<_>) = vector3 (-value.x, -value.y, -value.z)

    static member (*)(left: vector3<'u>, right: 'u) : vector3<'u> =
        vector3 (left.x * right, left.y * right, left.z * right)

    static member (/)(left: vector3<'u>, right: 'u) : vector3<'u> =
        vector3 (left.x / right, left.y / right, left.z + right)

    member this.Item
        with get (index: int) =
            match index with
            | 0 -> this.x
            | 1 -> this.y
            | 2 -> this.z
            | _ -> invalidArg "index" "Index out of range"
        and set (index: int) value =
            match index with
            | 0 -> this.x <- value
            | 1 -> this.y <- value
            | 2 -> this.z <- value
            | _ -> invalidArg "index" "Index out of range"

    member this.Item
        with get (c: Component) = this[int c]
        and set (c: Component) value = this[int c] <- value

    static member one = vector3 ('u.One, 'u.One, 'u.One)
    static member zero = vector3 ('u.Zero, 'u.Zero, 'u.Zero)
    static member AdditiveIdentity = vector3 ('u.Zero, 'u.Zero, 'u.Zero)
    static member MultiplicativeIdentity = vector3 ('u.One, 'u.One, 'u.One)
    static member size = 3

    override this.ToString() = $"({this.x}, {this.y}, {this.z})"

    interface IVector<vector3<'u>, 'u> with
        static member (+)(left: vector3<'u>, right: vector3<'u>) : vector3<'u> = left + right
        static member (-)(left: vector3<'u>, right: vector3<'u>) : vector3<'u> = left - right
        static member (*)(left: vector3<'u>, right: vector3<'u>) : vector3<'u> = left * right
        static member (/)(left: vector3<'u>, right: vector3<'u>) : vector3<'u> = left / right
        static member (~+)(value: vector3<_>) = value
        static member (~-)(value: vector3<_>) = -value
        static member one = vector3.one
        static member zero = vector3.zero
        static member AdditiveIdentity = vector3.AdditiveIdentity
        static member MultiplicativeIdentity = vector3.MultiplicativeIdentity
        static member size = vector3<'u>.size

        member this.Item
            with get index = this[index]
            and set index value = this[index] <- value

type int3 = vector3<int>
type float3 = vector3<float32>
type long3 = vector3<int64>

module int3 =
    let up = int3 (0, 1, 0)
    let right = int3 (1, 0, 0)
    let down = int3 (0, -1, 0)
    let left = int3 (-1, 0, 0)
    let forward = int3 (0, 0, 1)
    let back = int3 (0, 0, -1)
