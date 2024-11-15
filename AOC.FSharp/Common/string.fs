module String

open System

let split (separator: string) (str: string) : string array =
    str.Split(separator, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

let splitLines (str: string) : string array = split "\n" str
