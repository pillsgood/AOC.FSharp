module Seq

open System
open System.Linq

let addKey keySelector = Seq.map (fun x -> keySelector x, x) >> Map.ofSeq

let intersect (left: 'a seq) (right: 'a seq) : 'a seq = left.Intersect(right)

let minMap (selector: 'a -> #IComparable) (source: 'a seq) = source.Min(selector)

let filteri (predicate: int -> 'a -> bool) (source: 'a seq) =
    source |> Seq.indexed |> Seq.filter (fun (i, x) -> predicate i x) |> Seq.map snd

let zipAll (lists: 'a seq seq) : 'a seq seq =
    let enumerators = lists |> Seq.map _.GetEnumerator() |> Seq.toArray

    seq {
        while enumerators |> Seq.forall _.MoveNext() do
            yield enumerators |> Seq.map _.Current
    }
