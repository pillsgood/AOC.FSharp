[<AutoOpen>]
module AOC.FSharp.Common.Operators

let (?->) a b = if a then Some b else None

let (<&&>) f g x = f x && g x
let (<||>) f g x = f x || g x

let inline tap ([<InlineIfLambda>] f) x =
    f x
    x

let flip f x y = f y x

let inline konst x f = x
