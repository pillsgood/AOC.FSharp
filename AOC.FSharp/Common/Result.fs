namespace AOC.FSharp.Common

module Result =
    let guard value condition = if condition then Error value else Ok()
