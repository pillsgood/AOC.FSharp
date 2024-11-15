[<AutoOpen>]
module AOC.FSharp.Common.math

open System.Numerics

let inline epsilon< ^T & #IFloatingPointIeee754< ^T >> = 'T.Epsilon
