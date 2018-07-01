type ('a, 'b) map = Empty | Pair of 'a * 'b * ('a, 'b) map

let map_new () = Empty
let map_find =
    let rec aux s = function
        | Empty -> None
        | Pair (k, v, t) ->
                if k = s then Some v
                else aux s t
    in aux
let map_append k v m = Pair (k, v, m)

let rec fib n =
    if n = 0 then 0
    else if n = 1 then 1
    else fib (n - 1)  + fib (n - 2)

let fib_memo =
    let mem = ref @@ map_new () in
    mem := map_append 0 0 @@ map_append 1 1 !mem;
    let rec aux n =
        match map_find n !mem with
            | Some n -> n
            | None ->
                let x = aux (n - 1) + aux (n - 2) in
                mem := map_append n x !mem;
                x
    in aux

let fib_memo2 =
    let mem = Hashtbl.create 10 in
    Hashtbl.add mem 0 0; Hashtbl.add mem 1 1;
    let rec aux n =
        match Hashtbl.find_opt mem n with
            | Some n -> n
            | None ->
                let x = aux (n - 1) + aux (n - 2) in
                Hashtbl.add mem n x;
                x
    in aux

let tests () =
    let check a b t =
        if t then a = b else t
    in
        true
    |> check (fib 20) (fib_memo 20)
    |> check (fib_memo 20) (fib_memo2 20)

let bench () =
    let time name f x =
        let t = Sys.time () in
        ignore( f x );
        Printf.printf "%s( %d ) execution time: %fs\n" name x ( Sys.time () -. t )
    in
        time "fib_memo" fib_memo 50;
        time "fib_memo" fib_memo 50;
        time "fib_memo" fib_memo 500;
        time "fib_memo" fib_memo 500;
        time "fib_memo2" fib_memo2 50;
        time "fib_memo2" fib_memo2 50;
        time "fib_memo2" fib_memo2 500;
        time "fib_memo2" fib_memo2 500;

