module type PQUEUE =
sig
    type priority
    type 'a t

    exception EmptyPQueue

    val empty : 'a t
    val insert : 'a t -> priority -> 'a -> 'a t
    val remove : 'a t -> priority * 'a * 'a t
end

module PQueue : PQUEUE with type priority = int =
struct
    type priority = int
    type 'a t = ('a * priority) list

    exception EmptyPQueue

    let empty = []
    let rec insert q p e =
        let rec aux acc q p e =
            match q with
                | (ev, ep) :: tl ->
                    if p >= ep then
                        (List.rev acc) @ ((e, p) :: (ev, ep) :: tl)
                    else
                        aux ((ev, ep) :: acc) tl p e
                | [] -> (List.rev acc) @ [(e, p)]
        in aux [] q p e
    let remove = function
        | (ev, ep) :: tl -> (ep, ev, tl)
        | [] -> raise EmptyPQueue
end

module type ORDTYPE =
sig
    type t
    type comparison = LT | EQ | GT

    val compare : t -> t -> comparison
end

module IntOrd : ORDTYPE with type t = int =
struct
    type t = int
    type comparison = LT | EQ | GT

    let compare a b =
        if a < b then LT
        else if a > b then GT
        else EQ
end

module PQueueOrd (OrdType : ORDTYPE) =
struct
    type priority = OrdType.t
    type 'a t = ('a * priority) list

    exception EmptyPQueue

    let empty = []
    let rec insert q p e =
        let rec aux acc q p e =
            match q with
                | (ev, ep) :: tl ->
                    if (OrdType.compare ep p) = OrdType.LT then
                        (List.rev acc) @ ((e, p) :: (ev, ep) :: tl)
                    else
                        aux ((ev, ep) :: acc) tl p e
                | [] -> (List.rev acc) @ [(e, p)]
        in aux [] q p e
    let remove = function
        | (ev, ep) :: tl -> (ep, ev, tl)
        | [] -> raise EmptyPQueue

end

module PQueueInt = PQueueOrd(IntOrd)

let sort l =
    let rec insert_all q = function
        | hd :: tl -> insert_all (PQueue.insert q (hd + 0) hd) tl
        | [] -> q
    in
    let rec remove_all acc q =
        let (_, e, tl) = PQueue.remove q in
        if tl = PQueue.empty then
            e :: acc
        else
            remove_all (e :: acc) tl
    in
        remove_all [] (insert_all (PQueue.empty) l)

let sort2 l =
    let rec insert_all q = function
        | hd :: tl -> insert_all (PQueueInt.insert q (hd + 0) hd) tl
        | [] -> q
    in
    let rec remove_all acc q =
        let (_, e, tl) = PQueueInt.remove q in
        if tl = PQueueInt.empty then
            e :: acc
        else
            remove_all (e :: acc) tl
    in
        remove_all [] (insert_all (PQueueInt.empty) l)

let generic_sorter (type a) (module O : ORDTYPE with type t = a) l =
    let module Q = PQueueOrd( O ) in
    let rec insert_all q = function
        | hd :: tl -> insert_all (Q.insert q hd hd) tl
        | [] -> q
    in
    let rec remove_all acc q =
        let (_, e, tl) = Q.remove q in
        if tl = Q.empty then
            e :: acc
        else
            remove_all (e :: acc) tl
    in
        remove_all [] (insert_all (Q.empty) l)

let sort3 = generic_sorter (module IntOrd)

let tests () =
    let check a b t =
        if t then a = b else t
    in
    let s1 = [2;4;6;78;5;6;7;3;5;8;1] in
    let s2 = [3;7;6;9;4;5;2;34;-23;23;33;33;1;1] in
        true
    |>  check (List.sort compare s1) (sort s1)
    |>  check (List.sort compare s2) (sort s2)
    |>  check (List.sort compare s1) (sort2 s1)
    |>  check (List.sort compare s2) (sort2 s2)
    |>  check (List.sort compare s1) (sort3 s1)
    |>  check (List.sort compare s2) (sort3 s2)
