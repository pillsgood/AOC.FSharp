[<AutoOpen>]
module AOC.FSharp.Common.Operators

let (?->) a b = if a then Some b else None

let tap f x =
    f x
    x

let flip f x y = f y x

let inline konst x f = x

let inline (!) f x = not (f x)
let inline (!!) f x y = not (f x y)
let inline (!!!) f x y z = not (f x y z)

let (!=) = (<>)
