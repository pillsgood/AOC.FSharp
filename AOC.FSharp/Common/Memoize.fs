[<AutoOpen>]
module AOC.FSharp.Common.Memoize

let inline memoize f =
    let cache = System.Collections.Concurrent.ConcurrentDictionary()
    fun x -> cache.GetOrAdd(x, lazy f x).Value

let inline memoize2 f = memoize (fun (x, y) -> f x y) |> fun (f') x y -> f' (x, y)

let inline memoize3 f = memoize (fun (x, y, z) -> f x y z) |> fun f' x y z -> f' (x, y, z)

let memoizeRec f =
    let cache = System.Collections.Concurrent.ConcurrentDictionary()
    let rec f' x = cache.GetOrAdd(x, lazy f f' x).Value
    f'

let memoizeRec2 f =
    let cache = System.Collections.Concurrent.ConcurrentDictionary()
    let rec f' x y = cache.GetOrAdd((x, y), lazy f f' x y).Value
    f'
