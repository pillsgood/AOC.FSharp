module Seq

open System.Linq

let toMap keySelector =
    Seq.map (fun x -> keySelector x, x) >> Map.ofSeq

let intersect (left: 'a seq) (right: 'a seq) : 'a seq = left.Intersect(right)
