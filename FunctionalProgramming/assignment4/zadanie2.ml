type 'a mtree = MNode of 'a * 'a forest
 and 'a forest = EmptyForest | Forest of 'a mtree * 'a forest

type 'a mtree_lst = MTree of 'a * ('a mtree_lst) list

let bfs1 (MNode (e, f)) =
    let rec aux queue =
        match queue with
            | [] -> []
            | Forest (MNode (e, nf), f) :: tl -> e :: aux (f :: (tl @ [nf]) )
            | EmptyForest :: tl -> aux tl
    in e :: aux [f]

let rec bfs2 (MTree (e, f)) =
    let rec aux forest =
        if forest = [] then [] else
        let (values, queue) = List.fold_left (fun (v,q) (MTree (ee, ff)) -> (ee::v, q@ff)) ([], []) forest
        in (List.rev values) @ aux queue
    in e :: (aux f)

let dfs1 (MNode (e, f)) =
    let rec aux forest acc =
        match forest with
            | Forest ( MNode (a, f), ff ) -> a :: (aux f (aux ff acc))
            | EmptyForest -> acc
    in e :: aux f []

let rec dfs2 (MTree (e, f)) =
    e :: List.fold_left (@) [] (List.map dfs2 f)

let tests () =
    let t1 =
        MTree(2, [
            MTree(3, [
                MTree(5, [])
            ]);
            MTree(4, [
                MTree(6, [])
            ])
        ])
    in
    let t2 =
        MNode(2, Forest(
            MNode(3, Forest(
                MNode(5, EmptyForest),
                EmptyForest
            )), Forest(
            MNode(4, Forest(
                MNode(6, EmptyForest),
                EmptyForest
            )), EmptyForest))
        )
    in
    let check a b t =
        if t then a = b else t
    in
        true
    |> check (bfs2 t1) [2;3;4;5;6]
    |> check (bfs1 t2) [2;3;4;5;6]
    |> check (dfs2 t1) [2;3;5;4;6]
    |> check (dfs1 t2) [2;3;5;4;6]
