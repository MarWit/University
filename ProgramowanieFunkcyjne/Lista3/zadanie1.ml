let horner a x =
    let rec aux a acc x  =
        match a with
            | hd :: tl -> aux tl (x *. acc +. hd) x
            | [] -> acc
    in aux a 0. x

let horner2 a x =
    List.fold_left (fun acc v -> x *. acc +. v) 0. a

let tests () =
    let check a b r = if r then a = b else r in
        true
    |> check (horner [2.;3.;4.;5.] 5.) (horner [2.;3.;4.;5.] 5.)
    |> check (horner [1.;2.;1.;2.] 0.5) (horner2 [1.;2.;1.;2.] 0.5)
    |> check (horner [2.;3.;4.;5.] 5.) 350.
    |> check (horner2 [1.;2.;1.;2.] 0.5) 3.125

