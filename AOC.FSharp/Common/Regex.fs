namespace AOC.FSharp.Common

open System.Diagnostics.CodeAnalysis
open System.Text.RegularExpressions


module Regex =
    let matches ([<StringSyntax(StringSyntaxAttribute.Regex)>] pattern) input = Regex.Matches(input, pattern)

    let replace ([<StringSyntax(StringSyntaxAttribute.Regex)>] pattern) (replacement: string) input =
        Regex.Replace(input, pattern, replacement)

    module Group =
        let tryFindValue key (collection: GroupCollection) =
            match collection.TryGetValue(key) with
            | true, g when g.Success -> Some g.Value
            | _ -> None

        let tryFind key (collection: GroupCollection) =
            match collection.TryGetValue(key) with
            | true, g when g.Success -> Some g
            | _ -> None



[<AutoOpen>]
module RegexPattern =
    let (|MatchValue|_|) (pattern: Regex) input =
        let m = pattern.Match(input)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None

    let (|Match|_|) (pattern: Regex) input =
        let m = pattern.Match(input)
        if m.Success then Some(List.tail [ for g in m.Groups -> g ]) else None

    let (|Matches|_|) (pattern: Regex) (input: string) : Match list option =
        let matches = pattern.Matches(input)

        if matches.Count > 0 then Some([ for m in matches -> m ]) else None
