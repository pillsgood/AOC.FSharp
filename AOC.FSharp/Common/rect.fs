namespace AOC.FSharp.Common

open System.Numerics

[<Struct>]
type rect<'u & #INumber<'u>> =
    struct
        val mutable min: vector2<'u>
        val mutable max: vector2<'u>

        new(xMin, yMin, xMax, yMax) = { min = vector2 (xMin, yMin); max = vector2 (xMax, yMax) }

        new(min: vector2<'u>, size: vector2<'u>) =
            let max = min + size
            { min = min; max = max }

        new(min: vector2<'u>, width: 'u, height: 'u) =
            let max = min + vector2 (width, height)
            { min = min; max = max }

        new(rect: rect<_>, ?min: vector2<_>, ?max: vector2<_>) =
            let min = defaultArg min rect.min
            let max = defaultArg max rect.max
            { min = min; max = max }

        new(rect: rect<_>, ?xMin: 'u, ?yMin: 'u, ?xMax: 'u, ?yMax: 'u) =
            let xMin = defaultArg xMin rect.min.x
            let yMin = defaultArg yMin rect.min.y
            let xMax = defaultArg xMax rect.max.x
            let yMax = defaultArg yMax rect.max.y

            { min = vector2 (xMin, yMin); max = vector2 (xMax, yMax) }
    end

    static member minMax min max = rect (min, max - min)

    member this.xMin = this.min.x
    member this.yMin = this.min.y
    member this.xMax = this.max.x
    member this.yMax = this.max.y

    member this.width = this.xMax - this.xMin
    member this.height = this.yMax - this.yMin

    member this.perimeter = 'u.CreateChecked(2) * (this.width + this.height)

    member this.size = vector2 (this.width, this.height)

    override this.ToString() = $"rect {{ min: {this.min}, max: {this.max} }}"

module Rect =
    let contains (point: vector2<_>) (rect: rect<_>) =
        let x = point.x >= rect.xMin && point.x <= rect.xMax
        let y = point.y >= rect.yMin && point.y <= rect.yMax
        x && y

    let inline intersects (left: rect<_>) (right: rect<_>) =
        let x = left.xMin <= right.xMax && left.xMax >= right.xMin
        let y = left.yMin <= right.yMax && left.yMax >= right.yMin
        x && y

    let inline grow (size: vector2<'u>) (r: rect<'u>) =
        let min = r.min - size
        let max = r.max + size
        rect<'u>.minMax min max

type rectInt = rect<int>
