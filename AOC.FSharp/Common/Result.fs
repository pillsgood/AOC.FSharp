namespace AOC.FSharp.Common

module Result =
    let validate predicate value = if predicate value then Error() else Ok()
