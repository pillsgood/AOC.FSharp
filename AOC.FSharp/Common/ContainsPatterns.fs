[<AutoOpen>]
module CollectionPatterns

let inline (|Contains|_|) (item: 'b) (collection: 'a when 'a: (member Contains: 'b -> bool)) = collection.Contains(item)
