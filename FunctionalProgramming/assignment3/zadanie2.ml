let horner a x =
    let rec aux a acc x  =
        match a with
            | hd :: tl -> aux tl (x *. acc +. hd) x
            | [] -> acc
    in aux (List.rev a) 0. x

(* Poprawić bez odwracania *)
let horner2 a x =
    List.fold_left (fun acc v -> x *. acc +. v) 0. (List.rev a)

let horner3 a x =
    let rec aux a x =
        match a with
            | hd :: tl -> hd + x * aux tl x
            | [] -> 1
    in aux (List.tl a) x

let tests () =
    let check a b r = if r then a = b else r in
        true
    |> check (horner [5.;4.;3.;2.] 5.) (horner [5.;4.;3.;2.] 5.)
    |> check (horner [2.;1.;2.;1.] 0.5) (horner2 [2.;1.;2.;1.] 0.5)
    |> check (horner [5.;4.;3.;2.] 5.) 350.
    |> check (horner2 [2.;1.;2.;1.] 0.5) 3.125
