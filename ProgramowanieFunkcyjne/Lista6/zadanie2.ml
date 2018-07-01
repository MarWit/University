type 'a btree = Leaf of 'a | Node of 'a btree * 'a * 'a btree


let draw =
    let rec aux i = function
        | Node (l, x, r) -> Printf.printf "%s%d\n" i x;
                            aux (i^"    |") l;
                            aux (i^"    |") r
        | Leaf a -> Printf.printf "%s -- %d\n" i a
    in aux ""

let rec cnt_preorder t =
    let rec aux ?(n=1) = function
        | Leaf _ -> (Leaf n, n+1)
        | Node (l, a, r) ->
                let (nl, nn) = aux ~n:(n+1) l in
                let (nr, nn2) = aux ~n:nn r in
                ( Node (nl, n, nr ), nn2 )
    in fst @@ aux t

let tree =
  Node
   (Node (Node (Leaf 'x', 'x', Leaf 'x'), 'x', Node (Leaf 'x', 'x', Leaf 'x')),
   'x',
   Node (Node (Leaf 'x', 'x', Leaf 'x'), 'x', (Leaf 'x')));;

let cnt_bfs t =
    let strip_tree off i = function
        | Node (l, _, r) -> Node ( Leaf (-1), off + i, Leaf (-1) )
        | Leaf a -> Leaf (off + i)
    in
    let to_array = function
        | Node (l, _, r) -> [l; r]
        | Leaf _ -> []
    in
    let rec make_forest a n acc =
        if a = [] then acc else
        let next = List.concat @@ List.map to_array a in
        let current = List.mapi (strip_tree n) a in
        make_forest next (n + List.length a) (current::acc)
    in
    let rec connect_nodes acc root nodes =
        match (root, nodes) with
            | ([], _) -> acc
            | (Leaf a :: tl, _) -> connect_nodes (Leaf a :: acc) tl nodes
            | (Node (_, n, _) :: tl, a :: b :: nl) -> connect_nodes (Node (a, n, b) :: acc) tl nl
            | _ -> failwith "connect_node"
    in
    let rec join_forest b = function
        | hd :: tl -> join_forest (List.rev @@ connect_nodes [] hd b) tl
        | [] -> b
    in
    let forest = make_forest [t] 1 [] in
    match forest with
        | hd :: tl -> List.hd @@ join_forest hd tl
        | [] -> failwith "cnt_bfs"

let tests () =
    let check a b t =
        if t then a = b else t
    in
    let t = Node ( Node ( Leaf 'a', 'b', Leaf 'c' ), 'd', Leaf 'e' ) in
    let a1 = Node ( Node ( Leaf 3, 2, Leaf 4 ), 1, Leaf 5 ) in
    let a2 = Node ( Node ( Leaf 4, 2, Leaf 5 ), 1, Leaf 3 ) in
        true
    |> check (cnt_preorder t) a1
    |> check (cnt_bfs t) a2

