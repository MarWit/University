type htree = HLeaf of char * int | HNode of htree * htree * int

let weight = function
    | HLeaf (_, n) -> n
    | HNode (_, _, n) -> n

let merge t1 t2 = HNode (t1, t2, weight t1 + weight t2)

let buildHTree l =
    let rec aux = function
        | [x] -> x
        | a :: b :: [] -> merge a b
        | a :: b :: tl -> aux @@ List.sort (fun a b -> compare (weight a) (weight b)) (merge a b :: tl)
        | [] -> failwith "impossible"
    in aux @@ List.map (fun (a,b) -> HLeaf (a,b)) (List.sort (fun (_,a) (_,b) -> compare a b) l)

let getFreq s =
    let aux t c =
        match Hashtbl.find_opt t c with
            | Some i -> Hashtbl.replace t c (i+1)
            | None -> Hashtbl.add t c 1
    in
    let tbl = Hashtbl.create 0 in
    String.iter (aux tbl) s ;
    Hashtbl.fold ( fun a b -> List.cons (a, b) ) tbl []

let buildCodemap =
    let rec aux = function
        | HLeaf (c, w) -> [(c, ([0], 1))]
        | HNode (l, r, w) ->
                let add b (c,(l,n)) = (c, (b :: l, n+1)) in
                List.map (add 0) (aux l) @
                List.map (add 1) (aux r)
    in aux

let take_and_drop =
    let rec aux a l n =
        if n = 0 then (List.rev a, l)
        else match l with
            | hd :: tl -> aux (hd :: a) tl (n-1)
            | [] -> (List.rev a, [])
    in aux []

let rec insert l v n =
    if n = 0 then l
    else insert (v :: l) v (n-1)

let char_to_bits c =
    let rec aux n a c =
        if c = 0 then insert a 0 (8-n)
        else if (c land 1) <> 0 then
            aux (n+1) (1 :: a) (c lsr 1)
        else
            aux (n+1) (0 :: a) (c lsr 1)
    in aux 0 [] (Char.code c)

let bits_to_byte l =
    String.make 1 @@ Char.chr (List.fold_left (fun a e -> e + (lsl) a 1) 0 l)

let encode t s =
    let rec aux c s (a, q, n) =
        match Stream.peek s with
            | Some _ ->
                    let (code, len) = List.assoc (Stream.next s) c in
                    let (qq, nn) = (q @ code, n + len) in
                    if nn > 8 then
                        let (byte, queue) = take_and_drop qq 8 in
                        let char = bits_to_byte byte in
                        aux c s (a ^ char, queue, nn - 8)
                    else
                        aux c s (a, qq, nn)
            | None ->
                    if q <> [] then
                        let x = 8 - List.length q in
                        (String.make 1 @@ Char.chr x) ^ a ^ (bits_to_byte q)
                    else
                        (String.make 1 @@ Char.chr 0) ^ a
    in aux (buildCodemap t) s ("", [], 0) |> Stream.of_string

let rec drop l n =
    if n = 0 then l
    else match l with
        | hd :: tl -> drop tl (n-1)
        | [] -> []

let last_item s =
    match Stream.peek s with
        | Some _ -> false
        | None -> true

let decode t s =
    let rec find_matching ?(n=1) ?(acc=[]) c max l =
        match l with
            | [] -> None
            | hd :: tl ->
                let nacc = acc @ [hd] in
                match List.assoc_opt nacc c with
                    | Some c -> Some (String.make 1 c, tl)
                    | None -> if n < max then
                                find_matching ~n:(n+1) ~acc:nacc c max tl
                              else failwith "decode"
    in
    let rec aux n (c, max) s (a, q) =
        match Stream.peek s with
            | Some _ ->
                    let bits = char_to_bits (Stream.next s) in
                    let tbits = if last_item s then drop bits n else bits in
                    let qq = q @ tbits in
                    (match find_matching c max qq with
                        | Some (char, tl) -> aux n (c, max) s (a ^ char, tl)
                        | None -> aux n (c, max) s (a, qq))
            | None ->
                    if q <> [] then
                        (match find_matching c max q with
                            | Some (char, tl) -> aux n (c, max) s (a ^ char, tl)
                            | None -> a)
                    else a
    in
    let c = List.map (fun (c, (l, _)) -> (l, c)) (buildCodemap t) in
    let max = List.fold_left (fun a (l, _) -> let len = List.length l in if len > a then len else a) 0 c in
    let n = Char.code @@ Stream.next s in
    aux n (c, max) s ("", []) |> Stream.of_string

let rec stream_to_string ?(a="") s =
    match Stream.peek s with
        | Some _ -> stream_to_string ~a:(a ^ String.make 1 (Stream.next s)) s
        | None -> a

let load_file fin =
    let rec aux a s =
        match Stream.peek s with
            | Some c -> aux (a ^ (String.make 1 (Stream.next s))) s
            | None -> a
    in
    let f = open_in fin in
    let out = aux "" (open_in fin |> Stream.of_channel) in
    close_in f; out

let write_file fout d =
    let f = open_out fout in
    Printf.fprintf f "%s" d;
    close_out f

let encode_file fin fout =
    let str = load_file fin in
    let ht = buildHTree @@ getFreq str in
    let out = encode ht (Stream.of_string str) in
    write_file fout (stream_to_string out);
    ht

let decode_file ht fin fout =
    let str = load_file fin in
    let out = decode ht (Stream.of_string str) in
    write_file fout (stream_to_string out)

let tests () =
    let s1 = "Ala ma kota, a kot ma ale" in
    let s2 = "eeeeeeeeeeeeeeeeeeeeeeeee" in
    let f1 = "shit.ml" in
    let f2 = "encoded.dat" in
    let f3 = "decoded.dat" in
    let check a b t =
        if t then a = b else t
    in
    let h1 = buildHTree @@ getFreq s1 in
    let h2 = buildHTree @@ getFreq s2 in
    let h3 = encode_file f1 f2 in
    let s3 = load_file f1 in
    decode_file h3 f2 f3;
    ignore (    true
    |> check s1 (stream_to_string @@ decode h1 (encode h1 (Stream.of_string s1)))
    |> check s2 (stream_to_string @@ decode h2 (encode h2 (Stream.of_string s2)))
    |> check s3 (load_file f3));
    h3


let a = getFreq "eeeeeeeeeeeeeeeeeeeeeeeee";;
let b = buildHTree a;;
let c () = encode b (Stream.of_string "eeeeeeeeeeeeeeeeeeeeeeeee");;
let d () = decode b (c ())
