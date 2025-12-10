module String

open System

let inline split (separator: string) (str: string) : string array =
    str.Split(separator, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

let inline splitLines (str: string) : string array = split "\n" str

let inline endsWith (value: string) (str: string) = str.EndsWith(value)

let inline splitInto (count: int) (str: string) : string array =
    str |> Seq.splitInto count |> Seq.map String |> Seq.toArray

let inline toArray (str: string) : char array = str.ToCharArray()

let inline toList (str: string) : char list = str |> List.ofSeq

let inline split2 (str: string) : string * string =
    str |> toArray |> Array.splitInto 2 |> (fun l -> String(l[0]), String(l[1]))

let inline chunkBySize (count: int) (str: string) : string array =
    str |> Seq.chunkBySize count |> Seq.map String |> Seq.toArray

let inline join (separator: string) (str: string seq) = String.Join(separator, str)

let inline rev (str: string) = String(str.ToCharArray() |> Array.rev)
