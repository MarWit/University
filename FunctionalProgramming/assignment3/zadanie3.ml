let deriv a =
    let rec aux a acc n =
        if n = 0. then aux (List.tl a) acc 1.
        else match a with
            | hd :: tl -> aux tl (acc@[hd *. n]) (n+.1.)
            | [] -> acc
    in aux a [] 0.

let deriv2 a =
    List.fold_left (fun acc v -> acc@[v *. float_of_int ( 1 + List.length acc )]) [] (List.tl a)

let tests () =
    let check a b r = if r then a = b else r in
        true
    |> check (deriv [1.;2.;3.;4.]) (deriv2 [1.;2.;3.;4.])
    |> check (deriv [5.;2.;5.;2.;1.]) (deriv2 [5.;2.;5.;2.;1.])
    |> check [2.;6.;12.] (deriv [1.;2.;3.;4.])
    |> check [2.;10.;6.;4.] (deriv2 [5.;2.;5.;2.;1.])

