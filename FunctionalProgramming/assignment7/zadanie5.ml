type 'a lnode = {item: 'a; mutable next: 'a lnode}

let mk_circular_list e =
    let rec x = {item=e; next=x}
    in x

let insert_tail e l =
    let x = {item=e; next=l.next}
    in l.next <- x; x

let elim_head l = l.next <- (l.next).next; l
let mk_range_circular_list n =
    let l = ref @@ mk_circular_list 1 in
    for i = 2 to n do
        l := insert_tail i !l
    done; !l

let skip n l =
    for i = 1 to n do
        l := !l.next
    done; !l

let jozef n m =
    let table = ref @@ mk_range_circular_list n in
    let out = ref [] in
    for i = 1 to n do
        ignore @@ skip (m-1) table;
        out := !table.next.item :: !out;
        table := elim_head !table
    done; !out |> List.rev

let tests () =
    let check a b t =
        if t then a = b else t
    in
        true
    |> check (jozef 7 3) [3;6;2;7;5;1;4]

