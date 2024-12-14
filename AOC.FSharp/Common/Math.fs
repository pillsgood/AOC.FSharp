module Math

open System.Numerics

let inline epsilon< ^T & #IFloatingPointIeee754< ^T >> = 'T.Epsilon

let inline modulo a n = ((a % n) + n) % n
