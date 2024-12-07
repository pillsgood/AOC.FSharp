module Vector

open System.Numerics
open AOC.FSharp.Common

let inline components (vector: 'v & #IVector<_, 'u>) : ^u seq =
    seq {
        let size = 'v.size - 1

        for i = 0 to size do
            yield vector[i]
    }

let inline map (f: ^u -> ^u) (vector: 'v & #IVector<'v, 'u>) : 'v & #IVector<'v, 'u> =
    let size = 'v.size - 1
    let mutable result = vector

    for i = 0 to size do
        result[i] <- f vector[i]

    result

let inline sqrMagnitude (vector: ^v) : 'u = components vector |> Seq.map (fun x -> x * x) |> Seq.reduce (+)

let inline magnitude (vector: 'v & #IVector<_, 'u>) : 'r & #IRootFunctions<'r> = vector |> sqrMagnitude |> 'r.CreateChecked |> 'r.Sqrt

let inline manhattan (vector: 'v & #IVector<_, 'u>) : 'u & #INumber<'u> = components vector |> Seq.map 'u.Abs |> Seq.reduce (+)

let inline sqrDistance (a: ^v) (b: ^v) : 'u = sqrMagnitude (a - b)

let inline normalize (vector: 'v & #IVector<_, 'u>) = vector |> map (fun u -> 'u.Clamp(u, -'u.One, 'u.One))

let inline dot (left: ^v) (right: ^v) : 'u =
    let leftComponents = components left
    let rightComponents = components right

    Seq.zip leftComponents rightComponents
    |> Seq.map (fun (l, r) -> l * r)
    |> Seq.reduce (+)

let inline abs (vector: 'v & #IVector<'v, 'u>) : 'v = vector |> map 'u.Abs
