type 'a list_mutable = LMnil | LMcons of 'a * 'a list_mutable ref

let rec concat_copy a b =
    match a with
        | LMnil -> b
        | LMcons (e, t) -> LMcons (e, ref (concat_copy !t b))

 let rec concat_share a b =
     let rec aux a b =
        match !a with
            | LMnil -> a := !b
            | LMcons (_, t) -> aux t b
     in aux a b; !a

let rec to_mut = function
    | [] -> ref LMnil
    | hd :: tl -> ref @@ LMcons (hd, to_mut tl)

let string_of_mlist l =
    let rec aux s = function
        | LMnil -> s
        | LMcons (a, t) ->
                if !t = LMnil then
                    s ^ string_of_int a
                else
                    aux (s ^ string_of_int a ^ ", ") !t
    in
    "[" ^ aux "" l ^ "]"

let tests () =
    let check a b t =
        if t then a = b else t
    in
    let a = to_mut [1;2;3;4] in
    let b = to_mut [5;6;7;8] in
    let meh = ref LMnil in
    let a2 = a in
    let c1 = concat_copy !a !b in
    let c2 = concat_share a b in
    let c3 = concat_share meh b in
        true
    |> check (string_of_mlist c1) "[1, 2, 3, 4, 5, 6, 7, 8]"
    |> check (string_of_mlist c2) (string_of_mlist !a2)
    |> check (string_of_mlist !meh) (string_of_mlist !b)
