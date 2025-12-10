namespace AOC.FSharp.Common

open System.Numerics
open System.Runtime.CompilerServices

[<Struct>]
type RectInt<'u when 'u :> IBinaryInteger<'u>> =
    struct
        val mutable min: vector2<'u>
        val mutable max: vector2<'u>

        new(xMin, yMin, xMax, yMax) = { min = vector2 (xMin, yMin); max = vector2 (xMax, yMax) }

        new(min: vector2<'u>, size: vector2<'u>) =
            let max = min + (size - vector2<'u>.one)
            { min = min; max = max }

        new(min: vector2<'u>, width: 'u, height: 'u) =
            let max = min + vector2<'u> (width - 'u.One, height - 'u.One)
            { min = min; max = max }
    end

    static member minMax min max = RectInt(min, max - min)

    member this.xMin = this.min.x
    member this.yMin = this.min.y
    member this.xMax = this.max.x
    member this.yMax = this.max.y

    member this.width = 'u.Abs(this.xMax - this.xMin) + 'u.One
    member this.height = 'u.Abs(this.yMax - this.yMin) + 'u.One

    member this.perimeter = 'u.CreateChecked(2) * (this.width + this.height)

    member this.size = vector2<'u> (this.width, this.height)

    override this.ToString() = $"rectInt {{ min: {this.min}, max: {this.max} }}"

type rectInt = RectInt<int>

module RectInt =
    let contains (point: vector2<'u>) (rect: RectInt<'u>) =
        let x = point.x >= rect.xMin && point.x <= rect.xMax
        let y = point.y >= rect.yMin && point.y <= rect.yMax
        x && y

    let inline intersects (left: RectInt<'u>) (right: RectInt<'u>) =
        let x = left.xMin <= right.xMax && left.xMax >= right.xMin
        let y = left.yMin <= right.yMax && left.yMax >= right.yMin
        x && y

    let inline grow (size: vector2<'u>) (r: RectInt<'u>) : RectInt<'u> =
        let min = r.min - size
        let max = r.max + size
        rectInt.minMax min max

    let inline shrink (size: vector2<'u>) (r: RectInt<'u>) : RectInt<'u> = grow (-size) r

    let inline edgeContains (point: vector2<'u>) (rect: RectInt<'u>) =
        let x = point.x >= rect.xMin && point.x <= rect.xMax
        let y = point.y >= rect.yMin && point.y <= rect.yMax

        (x && (point.y = rect.yMin || point.y = rect.yMax))
        || (y && (point.x = rect.xMin || point.x = rect.xMax))

    let inline move (v: vector2<'u>) (rect: RectInt<'u>) = RectInt<'u>(rect.min + v, rect.size)
    let inline area (rect: RectInt<'u>) = (rect.width * rect.height)

[<AutoOpen>]
type RectIntExtensions =
    [<Extension>]
    static member Contains(rect: RectInt<'u>, point: vector2<'u>) = RectInt.contains point rect

    [<Extension>]
    static member Move(rect: RectInt<'u>, v: vector2<'u>) = RectInt.move v rect

    [<Extension>]
    static member Intersects(rect: RectInt<'u>, other: RectInt<'u>) = RectInt.intersects rect other
