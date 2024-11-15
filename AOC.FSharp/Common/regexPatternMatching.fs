[<AutoOpen>]
module AOC.FSharp.Common.regexPatternMatching

open System.Diagnostics.CodeAnalysis
open System.Text.RegularExpressions

let (|RegexMatch|_|) ([<StringSyntax("Regex")>] pattern: string) input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let (|RegexMatches|_|) ([<StringSyntax(StringSyntaxAttribute.Regex)>] pattern: string) input : Match list option =
    let matches = Regex.Matches(input, pattern)

    if matches.Count > 0 then
        Some([ for m in matches -> m ])
    else
        None
