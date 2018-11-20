type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree

let height tree =
    let rec aux tree =
        match tree with
            | Leaf -> 0
            | Node (ta, l, tb) -> 1 + max (aux ta) (aux tb)
    in aux tree

let is_balanced tree =
    let rec aux tree =
        match tree with
            | Leaf -> true
            | Node (ta, l, tb) ->
                    let sta = height ta in
                    let stb = height tb in
                    let diff = abs @@ sta - stb in
                    diff <= 1 && aux ta && aux tb
    in aux tree

(* ------- *)
let nodes_count tree =
    let rec aux tree =
        match tree with
            | Leaf -> 0
            | Node (ta, l, tb) -> 1 + (aux ta) + (aux tb)
    in aux tree

let is_balanced_ex tree =
    let rec aux tree cn =
        match tree with
            | Leaf -> true
            | Node (ta, l, tb) ->
                    let sta = nodes_count ta in
                    let stb = cn - sta in
                    let diff = abs @@ sta - stb in
                    diff <= 1 && aux ta (sta - 1) && aux tb (stb - 1)
    in aux tree (nodes_count tree - 1)

let half l =
    let rec aux l n acc =
        if n = 0 then (List.rev acc, l)
        else aux (List.tl l) (n - 1) (List.hd l :: acc)
    in aux l (List.length l / 2) []

let tree_from_list l =
    let rec aux l =
        match l with
            | hd :: tl ->
                    let (rl, ll) = half tl in
                    Node (aux rl, hd, aux ll)
            | [] -> Leaf
    in aux @@ List.sort compare l

let tests () =
    let check a b t =
        if t then a = b else t
    in
        true
    |>  check (tree_from_list [1;2;3;4;5;6;7] |> is_balanced) true
    |>  check (tree_from_list [8;5;6;2;3;4;1;4] |> is_balanced) true
