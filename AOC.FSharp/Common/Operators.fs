[<AutoOpen>]
module AOC.FSharp.Common.Operators

let (?->) a b = if a then Some b else None

let tap f x =
    f x
    x

let flip f x y = f y x

let negate f x = not (f x)
