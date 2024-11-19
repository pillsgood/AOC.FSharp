[<AutoOpen>]
module AOC.FSharp.Common.Operators

let (?->) a b = if a then Some b else None
