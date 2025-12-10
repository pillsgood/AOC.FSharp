module Option

let maybe x p = if p then Some x else None

let inline unfold f (x: 'a) : 'b option = f x
