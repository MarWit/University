let rec len ?(n=0) l =
    match l with
        | _ :: tl -> len ~n:(n+1) tl
        | [] -> n

let cycle l n =
    let rec aux l n =
        if n == 0 then l
        else match l with
                | hd :: tl -> aux (tl@[hd]) (n-1)
                | [] -> []
    in
        aux l ((len l) - n)

let tests () =
    let test a b =
        print_string (if (a=b) then "ok\n" else "fail\n")
    in
        test (cycle [1;2;3;4] 3) [2;3;4;1];
        test (cycle [1;2;3] 3) [1;2;3];
        test (cycle [1;2;3;4;5] 1) [5;1;2;3;4];
        test (cycle [2;4;2;4] 2) [2;4;2;4]

let () =
    tests ()
