let read_int str off len =
    let num = ref 0 in
    while !off < len && str.[ !off ] <> ' '
    do
        num := !num * 10 + (int_of_char str.[ !off ]) - 48 ;
        off := !off + 1
    done ;
    off := !off + 1 ;
    ! num

let read_first_line () =
    let line = read_line () in
    let off = ref 0 in
    let len = String.length line in

    let n = read_int line off len in
    let m = read_int line off len in
    let t = read_int line off len in
    (n, m, t)

let load_edge () =
    let line = read_line () in
    let off = ref 0 in
    let len = String.length line in

    let x1 = read_int line off len in
    let y1 = read_int line off len in
    let x2 = read_int line off len in
    let y2 = read_int line off len in

    ((x1, y1), (x2,y2))

let load_edges n =
    let rec aux n acc =
        if n > 0 then
            aux (n-1) (load_edge () :: acc)
        else acc
    in aux n []

let flip f a b = f b a

let find_opt map el =
    if Hashtbl.mem map el then
        Some (Hashtbl.find map el)
    else None

let create_or_add map el v =
    match find_opt map el with
        | None -> Hashtbl.add map el v
        | Some n -> Hashtbl.replace map el ((n + v) mod 999979)

let rec find_paths edges map =
    match edges with
        | (a, b) :: xl ->
            (match find_opt map b with
                | None -> find_paths xl map
                | Some n ->
                    create_or_add map a n;
                    find_paths xl map
            )
        | [] -> ()

let unwrap_or opt def =
    match opt with
        | Some n -> n
        | None -> def

let () =
    let (m, n, t) = read_first_line () in
    let edges = (flip List.fast_sort) (load_edges t) @@
        fun (va, _) (ua, _) ->
            let comp = compare (fst ua) (fst va) in
            if comp <> 0 then comp
            else compare (snd ua) (snd va)
    in
    let map = Hashtbl.create t in
    Hashtbl.add map (m, n) 1;
    find_paths edges map;
    print_int @@ unwrap_or (find_opt map (0,0)) 0
