type 'a place = Place of 'a list * 'a list

type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree
type 'a btplace = BTPlace of 'a btree * 'a btplace

let findNth l n =
    let rec aux l n acc =
        if n = 0 then (acc, l)
        else aux (List.tl l) (n-1) (List.hd l :: acc)
    in let s = aux l n []
    in Place (fst s, snd s)

let collapse (Place (a, b)) = List.rev a @ b

let add e (Place (a, b)) = Place (e :: a, b)
let del (Place (a, b)) = Place (List.tl a, b)
let next (Place (a, bh :: bt)) = Place (bh :: a, bt)
let prev (Place (ah :: at, b)) = Place (at, ah :: b)

let up (BTPlace (_, BTPlace (t, bt))) = BTPlace (t, bt)
let right (BTPlace( tree, bt ) ) =
    match tree with
        | Node (_, _, l) -> BTPlace ( l, BTPlace ( tree, bt ) )
        | Leaf -> BTPlace ( tree, bt )
let left (BTPlace( tree, bt ) ) =
    match tree with
        | Node (l, _, _) -> BTPlace ( l, BTPlace ( tree, bt ) )
        | Leaf -> BTPlace ( tree, bt )

let tests () =
    let check a b t =
        if t then a = b else t
    in
        true
    |> check (collapse (add 3 (findNth [1;2;4] 2))) [1;2;3;4]
    |> check (collapse (del (findNth [1;2;4] 2))) [1;4]
    |> check (del (add 9 (findNth [1;3;5;7;11;13] 4))) (findNth [1;3;5;7;11;13] 4)
