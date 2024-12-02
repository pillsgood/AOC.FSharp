[<AutoOpen>]
module Math

open System.Numerics

let inline epsilon< ^T & #IFloatingPointIeee754< ^T >> = 'T.Epsilon