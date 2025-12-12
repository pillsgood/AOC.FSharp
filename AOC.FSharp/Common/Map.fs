module Map

let findValue f (map: Map<'a, 'b>) = map.Values |> Seq.find f
let create (f: 'a -> 'b) (values: #seq<'a>) : Map<'b, 'a> = values |> Seq.map (fun x -> f x, x) |> Map.ofSeq
