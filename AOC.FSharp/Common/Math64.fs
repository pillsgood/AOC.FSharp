namespace AOC.FSharp.Common

[<AutoOpen>]
module Math64 =
    open System
    open System.Numerics

    type ulong = uint64
    let ulong value = uint64 value

    type BitVector64 =
        struct
            val mutable private _data: ulong

            new(data: ulong) = { _data = data }
            new(data: BitVector64) = { _data = data._data }

            member this.Data = this._data

            member this.Item
                with get (section: BitVector64Section): ulong =
                    let splice = this._data &&& (ulong section.Mask <<< section.Offset)
                    splice >>> section.Offset
                and set (section: BitVector64Section) (value: ulong) =
                    let value = value <<< section.Offset
                    let offsetMask = (ulong (uint Int32.MaxValue) &&& (ulong section.Mask)) <<< section.Offset
                    this._data <- (this._data &&& ~~~offsetMask) ||| (value &&& offsetMask)

            member this.Item
                with get (mask: uint, offset: int): ulong =
                    let mask = BitOperations.RoundUpToPowerOf2(mask + 1u) - 1u
                    let offset = (BitOperations.PopCount mask) * offset
                    let splice = this._data &&& (ulong mask <<< offset)
                    splice >>> offset
                and set (mask: uint, offset: int) (value: ulong) =
                    let mask = BitOperations.RoundUpToPowerOf2(mask + 1u) - 1u
                    let offset = (BitOperations.PopCount mask) * offset
                    let value = value <<< offset
                    let offsetMask = (ulong (uint Int32.MaxValue) &&& (ulong mask)) <<< offset
                    this._data <- (this._data &&& ~~~offsetMask) ||| (value &&& offsetMask)

            member this.Section
                with get (mask: uint, offset: int): BitVector64Section =
                    let mask = BitOperations.RoundUpToPowerOf2(mask + 1u) - 1u
                    let offset = (BitOperations.PopCount mask) * offset
                    BitVector64Section(mask, offset)

        end

    and BitVector64Section(mask: uint, offset: int) =
        struct
            member this.Mask: uint = mask
            member this.Offset: int = offset
        end

    module BitVector64 =
        let updateAt (selector: BitVector64 -> BitVector64Section) updater (v: BitVector64) =
            let mutable v = v
            let section = selector v
            let sectionValue = v[section]
            v[section] <- updater sectionValue
            v
