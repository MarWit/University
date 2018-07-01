let rec concat_map ?(a=[]) f l =
    match l with
        | [] -> a
        | hd :: tl -> concat_map ~a:(a@(f hd)) f tl

let rec map ?(a=[]) f l =
    match l with
        | [] -> a
        | hd :: tl -> map ~a:(a@[f hd]) f tl

let rec delete x l =
    match l with
        | [] -> []
        | hd :: tl -> if hd = x then tl else hd :: delete x tl

let rec permutations l =
    if l = [] then [[]]
    else
        concat_map (fun x -> map (fun n -> x::n) (permutations (delete x l))) l

let tests () =
    let test a b =
        print_string (if (a=b) then "ok\n" else "fail\n")
    in
    let rec factorial ?(a=1) n =
        if n == 0 then a
        else factorial (n-1) ~a:(a*n)
    in
    let cnt l = permutations l |> List.length in
        test (cnt [1;2;3;4]) (factorial 4);
        test (cnt [1;2;3;4;5]) (factorial 5);
        test (cnt [1;2;3;4;5;6;7;8]) (factorial 8)

let () =
    tests ()

