let rec suffix l =
    match l with
        | [] -> []
        | hd :: tl -> l::(suffix tl)

let prefix l =
    let rec aux a l =
        match l with
            | [] -> a :: []
            | hd :: tl -> a :: aux (a@[hd]) tl
    in
        match l with
            | [] -> [[]]
            | a :: l -> aux [a] l

let tests () =
    let test a b =
        print_string (if (a=b) then "ok\n" else "fail\n")
    in
        test (suffix [1;2;3;4]) [[1;2;3;4];[2;3;4];[3;4];[4]];
        test (suffix [1]) [[1]];
        test (suffix [1;2;3;4;5]) [[1;2;3;4;5];[2;3;4;5];[3;4;5];[4;5];[5]];
        test (suffix [2;4;2;4]) [[2;4;2;4];[4;2;4];[2;4];[4]];

        test (prefix [1;2;3;4]) [[1];[1;2];[1;2;3];[1;2;3;4]];
        test (prefix [1]) [[1]];
        test (prefix [1;2;3;4;5]) [[1];[1;2];[1;2;3];[1;2;3;4];[1;2;3;4;5]];
        test (prefix [2;4;2;4]) [[2];[2;4];[2;4;2];[2;4;2;4]]

let () =
    tests ()

