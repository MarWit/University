type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist)

let check_edges a b =
    let rec aux acc = function
        | Leaf e -> e :: acc
        | Node (l, r) -> aux (aux acc r) l
    in (aux [] a) = (aux [] b)

let rec lcompare = function
    | (LCons (a, ca), LCons(b, cb)) ->
            if a <> b then false
            else lcompare (ca (), cb ())
    | (LNil, LNil) -> true
    | (_, _) -> false

let check_edges_lazy a b =
    let rec aux c = function
        | Leaf e -> LCons (e, c)
        | Node (l, r) -> aux ( fun () -> aux c r ) l
    in
        let la = aux (fun () -> LNil) a in
        let lb = aux (fun () -> LNil) b in
        lcompare (la, lb)

let tests () =
    let check a b t =
        if t then a = b else t
    in
    let t1 = Node ( Node (Leaf 1, Leaf 2), Leaf 3 ) in
    let t2 = Node ( Leaf 1, Node (Leaf 2, Leaf 3 ) ) in
        true
    |> check (check_edges t1 t2) true
    |> check (check_edges_lazy t1 t2) true
