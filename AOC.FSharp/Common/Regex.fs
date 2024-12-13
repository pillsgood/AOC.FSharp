[<AutoOpen>]
module RegexExt

open System.Diagnostics.CodeAnalysis
open System.Text.RegularExpressions

type private Rgx = Regex

type Regex =
    static member matches ([<StringSyntax(StringSyntaxAttribute.Regex)>] pattern) input =
        Regex.Matches(input, pattern) |> Seq.toArray

    static member replace ([<StringSyntax(StringSyntaxAttribute.Regex)>] pattern: string) (replacement: string) (input: string) =
        Regex.Replace(input, pattern, replacement)

type System.Text.RegularExpressions.Regex with
    member inline this.replace (replacement: string) (input: string) = this.Replace(input, replacement)

[<AutoOpen>]
module RegexActivePattern =
    let (|MatchValue|_|) (pattern: Rgx) input : string list option =
        let m = pattern.Match(input)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ]) else None

    let (|Match|_|) (pattern: Rgx) input : Group list option =
        let m = pattern.Match(input)
        if m.Success then Some(List.tail [ for g in m.Groups -> g ]) else None

    let (|Matches|_|) (pattern: Rgx) (input: string) : Match list option =
        let matches = pattern.Matches(input)

        if matches.Count > 0 then Some([ for m in matches -> m ]) else None

    let (|MatchesValue|_|) (pattern: Rgx) (input: string) : string list list option =
        let matches = pattern.Matches(input)

        if matches.Count > 0 then
            Some([ for m in matches -> List.tail [ for g in m.Groups -> g.Value ] ])
        else
            None
