module FSharp.Collections.ParallelSeq.PSeq


let inline count predicate source = source |> PSeq.filter predicate |> PSeq.length
