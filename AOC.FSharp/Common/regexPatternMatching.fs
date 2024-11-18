[<AutoOpen>]
module AOC.FSharp.Common.regexPatternMatching

open System.Diagnostics.CodeAnalysis
open System.Text.RegularExpressions

let (|Match|_|) (pattern: Regex) input =
    let m = pattern.Match(input)

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let (|Matches|_|) (pattern: Regex) (input: string) : Match list option =
    let matches = pattern.Matches(input)

    if matches.Count > 0 then
        Some([ for m in matches -> m ])
    else
        None
