module Map

let findValue f (map: Map<'a, 'b>) = map.Values |> Seq.find f
