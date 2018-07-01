let may f o =
    match o with
        | Some e -> f e
        | None -> None

let find_i a =
    let rec aux a =
        match a with
            | a :: b :: tl -> if a > b then Some b
                              else aux (b :: tl)
            | _ -> None
    in aux a

let find_l a i = List.find ((<) i) a
let swap l a b = List.map (fun s -> if s = a then b else if s = b then a else s) l
let rev_after a i =
    let rec aux a i acc =
        match a with
            | hd :: tl -> if hd = i then tl@(hd :: acc)
                          else aux tl i (hd :: acc)
            | [] -> acc
    in aux (List.rev a) i []


let next_permutation a =
    let i = find_i a in
    may ( fun i ->
        let l = find_l a i in
        let a = swap a i l in
        Some ( rev_after a l )
    ) i

let permutations a =
    let rec aux a acc =
        match next_permutation a with
            | Some l -> aux l (acc@[l])
            | None -> acc
    in a :: aux (List.sort (fun a b -> if a < b then 1 else -1) a) []

let tests () =
    let check a b r = if r then a = b else r in
        true
    |>  check (List.length (permutations [1;2])) 2
    |>  check (List.length (permutations [1;2;3])) 6
    |>  check (List.length (permutations [1;2;3;4])) 24
    |>  check (List.length (permutations [1;2;3;4;5])) 120
