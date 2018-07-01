let rec foldr f c l =
    match l with
        | [] -> c
        | hd :: tl -> f hd (foldr f c tl)

let rec subsequences l =
    match l with
        | [] -> []
        | hd :: tl -> let f l r = l :: (hd::l) :: r in
                            [hd] :: foldr f [] (subsequences tl)

let tests () =
    let test a b =
        print_string (if (a=b) then "ok\n" else "fail\n")
    in
    let (^^) a n = truncate ((float_of_int a) ** (float_of_int n)) in
        test (subsequences [1;2]) [[1];[2];[1;2]];
        test (List.length (subsequences [1;2;3])) ((2^^3) - 1);
        test (List.length (subsequences [1;2;3;4;5])) ((2^^5) - 1);
        test (List.length (subsequences [1;2;3;4;5;6;7;8])) ((2^^8) - 1)

let () =
    tests ()
