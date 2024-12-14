module Seq

open System
open System.Collections.Generic
open System.Linq

let addKey keySelector = Seq.map (fun x -> keySelector x, x) >> Map.ofSeq

let intersect (left: 'a seq) (right: 'a seq) : 'a seq = left.Intersect(right)

let minMap (selector: 'a -> #IComparable) (source: 'a seq) = source.Min(selector)

let inline count predicate source = source |> Seq.filter predicate |> Seq.length

let filteri (predicate: int -> 'a -> bool) (source: 'a seq) =
    source |> Seq.indexed |> Seq.filter (fun (i, x) -> predicate i x) |> Seq.map snd

let splitBy predicate source =
    seq {
        let mutable acc = new List<'a>()

        for x in source do
            if predicate x then
                if acc.Count > 0 then yield acc.ToArray()
                acc.Clear()
                acc.Add(x)
            else
                acc.Add(x)

        yield acc.ToArray()
    }

let zipAll (lists: 'a seq seq) : 'a seq seq =
    let enumerators = lists |> Seq.map _.GetEnumerator() |> Seq.toArray

    seq {
        while enumerators |> Seq.forall _.MoveNext() do
            yield enumerators |> Seq.map _.Current
    }

let scanUnfold generator state =
    seq {
        let mutable opt = generator state None

        while Option.isSome opt do
            let current, next = opt.Value
            yield current
            opt <- generator next (Some current)
    }

let inline allEqualBy f source =
    match Seq.tryHead source with
    | Some first ->
        let first = f first
        source |> Seq.forall (fun x -> f x = first)
    | None -> failwith "Source sequence is empty."

let takeUntil predicate (source: seq<_>) =
    seq {
        use e = source.GetEnumerator()
        let mutable condition = true

        while e.MoveNext() && condition do
            let latest = e.Current
            yield latest
            condition <- predicate latest
    }

let inline reducei ([<InlineIfLambda>] f) (source: 'a seq) =
    use enumerator = source.GetEnumerator()

    if not <| enumerator.MoveNext() then
        invalidArg "source" "The input sequence was empty."

    let mutable acc = enumerator.Current
    let mutable index = 0

    while enumerator.MoveNext() do
        acc <- f acc enumerator.Current index
        index <- index + 1

    acc

let inline combinations n (source: 'a seq) =
    let rec combine acc n l =
        match n, l with
        | 0, _ -> [ List.rev acc ]
        | _, [] -> []
        | k, x :: xs -> combine (x :: acc) (k - 1) xs @ combine acc k xs

    combine [] n (List.ofSeq source)

let inline combinePairs (source: 'a seq) = source |> combinations 2 |> Seq.map (fun l -> l[0], l[1])

let inline tap f (source: 'a seq) =
    seq {
        for x in source do
            f x
            yield x
    }