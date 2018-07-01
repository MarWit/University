module type VERTEX =
sig
    type t
    type label

    val equal : t -> t -> bool
    val create : label -> t
    val label : t -> label
end

module type EDGE =
sig
    type t
    type v
    type label

    val equal : t -> t -> bool
    val create : label -> v -> v -> t
    val label : t -> label
    val first : t -> v
    val last : t -> v
end

module Vertex : VERTEX with type label = string =
struct
    type label = string
    type t = label

    let equal a b = a = b
    let create l = l
    let label v = v
end

module Edge : EDGE with type label = int and type v = Vertex.t =
struct
    type label = int
    type v = Vertex.t
    type t = v * label * v

    let equal a b = a = b
    let create l a b = (a, l, b)
    let label (_, l, _) = l
    let first (a, _, _) = a
    let last (_, _, b) = b
end

module type GRAPH =
sig
    type t

    module V : VERTEX
    type vertex = V.t

    module E : EDGE with type v = vertex

    type edge = E.t

    val mem_v : t -> vertex -> bool
    val mem_e : t -> edge -> bool
    val mem_e_v : t -> vertex -> vertex -> bool
    val find_e : t -> vertex -> vertex -> edge
    val succ : t -> vertex -> vertex list
    val pred : t -> vertex -> vertex list
    val succ_e : t -> vertex -> edge list
    val pred_e : t -> vertex -> edge list

    val empty : t
    val add_e : t -> edge -> t
    val add_v : t -> vertex -> t
    val rem_e : t -> edge -> t
    val rem_v : t -> vertex -> t

    val fold_v : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
end

let assoc_pop =
    let rec aux acc f k = function
        | (key, value) :: tl ->
                if f k key then (Some value, acc @ tl)
                else aux ((key, value) :: acc) f k tl
        | [] -> (None, acc)
    in aux []

module Graph : GRAPH with module V = Vertex and module E = Edge =
struct
    module V = Vertex
    type vertex = V.t

    module E = Edge
    type edge = E.t

    type t = (vertex * (edge list)) list

    let mem_v g v = List.exists ( fun (v', _) -> V.equal v v' ) g
    let mem_e g e =
        let v = E.first e in
        match List.find_opt ( fun (v', _) -> v = v' ) g with
            | Some (_, l) -> List.exists ( fun e' -> E.equal e e' ) l
            | None -> false
    let mem_e_v g v1 v2 =
        match List.find_opt (fun (v', _) -> V.equal v1 v') g with
            | Some (_, l) -> List.exists ( fun e' -> V.equal (E.last e') v2 ) l
            | None -> false

    let find_e g v1 v2 =
        match List.find_opt (fun (v', _) -> V.equal v1 v') g with
            | Some (_, l) ->
                    (match List.find_opt ( fun e' -> V.equal (E.last e') v2 ) l with
                        | Some e -> e
                        | None -> failwith "Graph.find")
            | None -> failwith "Graph.find"

    let succ g v =
        match List.find_opt (fun (v', _) -> V.equal v v') g with
            | Some (_, l) -> List.map E.last l
            | None -> failwith "Graph.succ"

    let pred g v =
        List.find_all (
            fun (_, l) -> List.exists ( fun e -> V.equal (E.last e) v ) l
        ) g |> List.map fst

    let succ_e g v =
        match List.assoc_opt v g with
            | Some l -> l
            | None -> failwith "Graph_succ.e"

    let pred_e g v =
        List.concat @@ List.map ( fun (_, l) -> List.filter (fun e -> V.equal (E.last e) v) l) g

    let empty = []
    let add_e g e =
        let g1 = (
            let v = E.last e in
            if List.exists ( fun (v', _) -> V.equal v' v ) g then
                g
            else
                (v, []) :: g
        ) in
        let v = E.first e in
        let (edges, rest) = assoc_pop V.equal v g1 in
            match edges with
                | Some l -> (v, (e :: l)) :: rest
                | None -> (v, [e]) :: g1
    let add_v g v =
        if List.exists (fun (v', _) -> V.equal v v') g then
            g
        else
            (v, []) :: g

    let rem_e g e =
        let v = E.first e in
        let (edges, rest) = assoc_pop V.equal v g in
        match edges with
            | Some l -> (v, ( List.filter ( fun a -> not @@ E.equal a e ) l )) :: rest
            | None -> g

    let rem_v g v =
        let (_, rest) = assoc_pop V.equal v g in
        List.map ( fun (v', l) -> (v', List.filter (fun e -> not @@ V.equal (E.last e) v) l) ) rest

    let fold_v f = List.fold_right (fun (v, _) acc -> f v acc)
    let fold_e f g = List.fold_right f (List.concat @@ List.map snd g)
end

module AnyGraph (Ver: VERTEX) (Ed: EDGE with type v = Ver.t) : GRAPH =
struct
    module V = Ver
    type vertex = V.t

    module E = Ed
    type edge = E.t

    type t = (vertex * (edge list)) list

    let __assoc_pop =
        let rec aux acc f k = function
            | (key, value) :: tl ->
                    if f k key then (Some value, acc @ tl)
                    else aux ((key, value) :: acc) f k tl
            | [] -> (None, acc)
        in aux []

    let mem_v g v = List.exists ( fun (v', _) -> V.equal v v' ) g
    let mem_e g e =
        let v = E.first e in
        match List.find_opt ( fun (v', _) -> v = v' ) g with
            | Some (_, l) -> List.exists ( fun e' -> E.equal e e' ) l
            | None -> false
    let mem_e_v g v1 v2 =
        match List.find_opt (fun (v', _) -> V.equal v1 v') g with
            | Some (_, l) -> List.exists ( fun e' -> V.equal (E.last e') v2 ) l
            | None -> false

    let find_e g v1 v2 =
        match List.find_opt (fun (v', _) -> V.equal v1 v') g with
            | Some (_, l) ->
                    (match List.find_opt ( fun e' -> V.equal (E.last e') v2 ) l with
                        | Some e -> e
                        | None -> failwith "Graph.find")
            | None -> failwith "Graph.find"

    let succ g v =
        match List.find_opt (fun (v', _) -> V.equal v v') g with
            | Some (_, l) -> List.map E.last l
            | None -> failwith "Graph.succ"

    let pred g v =
        List.find_all (
            fun (_, l) -> List.exists ( fun e -> V.equal (E.last e) v ) l
        ) g |> List.map fst

    let succ_e g v =
        match List.assoc_opt v g with
            | Some l -> l
            | None -> failwith "Graph_succ.e"

    let pred_e g v =
        List.concat @@ List.map ( fun (_, l) -> List.filter (fun e -> V.equal (E.last e) v) l) g

    let empty = []
    let add_e g e =
        let g1 = (
            let v = E.last e in
            if List.exists ( fun (v', _) -> V.equal v' v ) g then
                g
            else
                (v, []) :: g
        ) in
        let v = E.first e in
        let (edges, rest) = __assoc_pop V.equal v g1 in
            match edges with
                | Some l -> (v, (e :: l)) :: rest
                | None -> (v, [e]) :: g1
    let add_v g v =
        if List.exists (fun (v', _) -> V.equal v v') g then
            g
        else
            (v, []) :: g

    let rem_e g e =
        let v = E.first e in
        let (edges, rest) = __assoc_pop V.equal v g in
        match edges with
            | Some l -> (v, ( List.filter ( fun a -> not @@ E.equal a e ) l )) :: rest
            | None -> g

    let rem_v g v =
        let (_, rest) = __assoc_pop V.equal v g in
        List.map ( fun (v', l) -> (v', List.filter (fun e -> not @@ V.equal (E.last e) v) l) ) rest

    let fold_v f = List.fold_right (fun (v, _) acc -> f v acc)
    let fold_e f g = List.fold_right f (List.concat @@ List.map snd g)
end

let dfs (module G: GRAPH) =
    let rec aux a f g v =
        ignore( f v );
        let aa = v :: a in
        let to_check = Graph.succ g v in
        List.iter (fun v -> aux aa f g v) (List.filter (fun v' -> not @@ List.exists (Vertex.equal v') aa ) to_check)
    in aux []

let bfs (module G: GRAPH) f g v =
    let rec aux f g v =
        if g <> Graph.empty then
            let to_check = Graph.succ g v in
            List.iter f to_check;
            let ng = Graph.rem_v g v in
            List.iter (fun v -> aux f ng v) to_check
        else ()
    in ignore (f v); aux f g v

(*
 *     graph2
 *       a
 *    ./   \.
 *    b     c
 *  ./ \. ./ \.
 *  d   e f   g
 *
 *)

let tests () =
    let check a b t =
        if t then a = b else t
    in
    let get_graph1 () =
        let v1 = Vertex.create "a" in
        let v2 = Vertex.create "b" in
        let v3 = Vertex.create "c" in
        let v4 = Vertex.create "d" in
        let e1 = Edge.create 1 v1 v2 in
        let e2 = Edge.create 2 v2 v3 in
        let e3 = Edge.create 3 v3 v1 in
        let e4 = Edge.create 4 v1 v4 in
        let g = Graph.empty in
        let g1 = Graph.add_e g e1 in
        let g2 = Graph.add_e g1 e2 in
        let g3 = Graph.add_e g2 e3 in
        let g4 = Graph.add_e g3 e4 in
        (v1, g4)
    in
    let get_graph2 () =
        let v1 = Vertex.create "a" in
        let v2 = Vertex.create "b" in
        let v3 = Vertex.create "c" in
        let v4 = Vertex.create "d" in
        let v5 = Vertex.create "e" in
        let v6 = Vertex.create "f" in
        let v7 = Vertex.create "g" in
        let e1 = Edge.create 1 v1 v2 in
        let e2 = Edge.create 2 v1 v3 in
        let e3 = Edge.create 3 v2 v4 in
        let e4 = Edge.create 4 v2 v5 in
        let e5 = Edge.create 5 v3 v6 in
        let e6 = Edge.create 6 v3 v7 in
        let g = Graph.empty in
        let g1 = Graph.add_e g e1 in
        let g2 = Graph.add_e g1 e2 in
        let g3 = Graph.add_e g2 e3 in
        let g4 = Graph.add_e g3 e4 in
        let g5 = Graph.add_e g4 e5 in
        let g6 = Graph.add_e g5 e6 in
        (v1, g6)
    in
    let (v1, graph1) = get_graph1 () in
    let (v2, graph2) = get_graph2 () in
    let graph3 = Graph.rem_v graph1 v1 in
    print_string "     a\n";
    print_string "  ./   \.\n";
    print_string "  b     c\n";
    print_string "./ \. ./ \.\n";
    print_string "d   e f   g\n";
    print_string "DFS\n";
    dfs (module Graph) (fun v -> Printf.printf "%s -> " (Vertex.label v)) graph2 v2;
    print_string "END\nBFS\n";
    bfs (module Graph) (fun v -> Printf.printf "%s -> " (Vertex.label v)) graph2 v2;
    print_string "END\n";
        true
    |> check (List.sort compare @@ Graph.fold_v (fun v a -> ( Graph.V.label v ) :: a) graph1 []) ["a"; "b"; "c"; "d"]
    |> check (List.sort compare @@ Graph.fold_v (fun v a -> ( Graph.V.label v ) :: a) graph2 []) ["a"; "b"; "c"; "d"; "e"; "f"; "g"]
    |> check (List.sort compare @@ Graph.fold_v (fun v a -> ( Graph.V.label v ) :: a) graph3 []) ["b"; "c"; "d"]
