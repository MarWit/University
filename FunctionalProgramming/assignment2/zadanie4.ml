let rec range ?(a=0) ?(step=1) ?(acc=[]) b =
        if a + step > b then acc @ [a]
        else range ~a:(a+step) ~step:step ~acc:(acc@[a]) b

let rec merge f a b =
    match a, b with
        | [], _ -> b
        | _, [] -> a
        | ha::ta, hb::tb ->
                if f ha hb then
                    ha :: merge f ta b
                else
                    hb :: merge f a tb

let rec merge_tail ?(acc=[]) f a b =
    match a, b, acc with
        | [], [], acc -> acc
        | [], _, _ -> acc@b
        | _, [], _ -> acc@a
        | ha::ta, hb::tb, _ ->
                if f ha hb then
                    merge_tail ~acc:(acc@[ha]) f ta b
                else
                    merge_tail ~acc:(acc@[hb]) f a tb

let rec len ?(n=0) l =
    match l with
        | [] -> n
        | _ :: l -> len ~n:(n+1) l

let half l =
    let rec aux ?(a=[]) l n =
        if n == 0 then (a,l)
        else match l with
                | hd :: tl -> aux ~a:(a@[hd]) tl (n-1)
                | [] -> ([], [])
    in aux l ((len l)/2)

let rec merge_sort f a =
    match a with
        | [] -> []
        | [a] -> [a]
        | _ -> let aa, ab = half a in merge_tail f (merge_sort f aa) (merge_sort f ab)

let tests () =
    let test a b =
        print_string (if (a=b) then "ok\n" else "fail\n")
    in
        test (merge (<=) [1;3;5;7;9] [2;4;6;8;10]) (range ~a:1 10);
        test (merge (<=) [1;2;3;4;5] [1;2;3;4;5]) [1;1;2;2;3;3;4;4;5;5];
        test (merge (>) [5;3;1] [4;2;0]) [5;4;3;2;1;0];
        test (merge_tail (<=) [1;3;5;7;9] [2;4;6;8;10]) (range ~a:1 10);
        test (merge_tail (<=) [1;2;3;4;5] [1;2;3;4;5]) [1;1;2;2;3;3;4;4;5;5];
        test (merge_tail (>) [5;3;1] [4;2;0]) [5;4;3;2;1;0];
        test (merge_sort (<=) [4;7;4;2;8;0;123;34;2]) [0;2;2;4;4;7;8;34;123];
        test (merge_sort (<=) ['O'; 'c'; 'a'; 'm'; 'l']) ['O'; 'a'; 'c'; 'l'; 'm']

let () =
    tests ()
