namespace AOC.FSharp.Common

open System.Runtime.CompilerServices

[<Struct>]
type RectInt =
    struct
        val mutable min: int2
        val mutable max: int2

        new(xMin, yMin, xMax, yMax) = { min = vector2 (xMin, yMin); max = vector2 (xMax, yMax) }

        new(min: int2, size: int2) =
            let max = min + (size - int2.one)
            { min = min; max = max }

        new(min: int2, width: int, height: int) =
            let max = min + vector2 (width - 1, height - 1)
            { min = min; max = max }
    end

    static member minMax min max = rect (min, max - min)

    member this.xMin = this.min.x
    member this.yMin = this.min.y
    member this.xMax = this.max.x
    member this.yMax = this.max.y

    member this.width = (this.xMax - this.xMin) + 1
    member this.height = (this.yMax - this.yMin) + 1

    member this.perimeter = 2 * (this.width + this.height)

    member this.size = vector2 (this.width, this.height)

    override this.ToString() = $"rectInt {{ min: {this.min}, max: {this.max} }}"

type rectInt = RectInt

module RectInt =
    let contains (point: int2) (rect: rectInt) =
        let x = point.x >= rect.xMin && point.x <= rect.xMax
        let y = point.y >= rect.yMin && point.y <= rect.yMax
        x && y

    let inline intersects (left: rectInt) (right: rectInt) =
        let x = left.xMin <= right.xMax && left.xMax >= right.xMin
        let y = left.yMin <= right.yMax && left.yMax >= right.yMin
        x && y

    let inline grow (size: int2) (r: rectInt) =
        let min = r.min - size
        let max = r.max + size
        rectInt.minMax min max

    let inline edgeContains (point: int2) (rect: rectInt) =
        let x = point.x >= rect.xMin && point.x <= rect.xMax
        let y = point.y >= rect.yMin && point.y <= rect.yMax

        (x && (point.y = rect.yMin || point.y = rect.yMax))
        || (y && (point.x = rect.xMin || point.x = rect.xMax))

    let inline move (v: int2) (rect: rectInt) = rectInt (rect.min + v, rect.size)

[<AutoOpen>]
type RectIntExtensions =
    [<Extension>]
    static member Contains(rect: rectInt, point: int2) = RectInt.contains point rect

    [<Extension>]
    static member Move(rect: rectInt, v: int2) = RectInt.move v rect

    [<Extension>]
    static member Intersects(rect: rectInt, other: rectInt) = RectInt.intersects rect other
