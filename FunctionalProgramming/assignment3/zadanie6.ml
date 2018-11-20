let product (x,y) = x * y
let sum (x,y) = x + y

let rec range100 ?(n=100) ?(acc=[]) () =
    if n = 1 then 1 :: acc
    else range100 ~n:(n-1) ~acc:(n::acc) ()

let inter a b = List.filter (fun e -> List.exists ((=) e) b) a

let statement1 () =
    let range = range100 () in
    let p = List.flatten @@ List.map (fun a -> List.map (fun b -> (a, b) ) range ) range in
    List.filter (fun (a, b) -> 1 < a && a < b && (a + b) < 100) p

let sumEq p = List.filter (fun q -> sum q = sum p)
let productEq p = List.filter (fun q -> product q = product p)

let statement2 () =
    let s1 = statement1 () in
    List.filter (fun p -> List.for_all (fun q -> (List.length @@ productEq q s1) <> 1) (sumEq p s1)) s1

let statement3 () =
    let s1 = statement1 () in
    let s2 = statement2 () in
    List.filter (fun p -> (List.length (inter (productEq p s1) s2)) = 1) s2

let statement4 () =
    let s1 = statement1 () in
    let s3 = statement3 () in
    List.filter (fun p -> (List.length (inter (sumEq p s1) s3)) = 1) s3

let print_array_of_tuples a =
    print_string "[";
    ignore (List.map ( fun t -> Printf.printf "(%d, %d), " (fst t) (snd t) ) a);
    print_string "]"

let main () =
    print_array_of_tuples @@ statement4 ()
