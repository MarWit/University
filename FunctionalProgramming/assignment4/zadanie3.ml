type lformula =
    LTrue                       |
    LFalse                      |
    LVariable of char           |
    LNot of lformula            |
    LAnd of lformula * lformula |
    LOr of lformula * lformula

let rec string_of_formula p =
    match p with
        | LTrue -> "t"
        | LFalse -> "f"
        | LVariable c -> String.make 1 c
        | LAnd (a, b) -> "(" ^ (string_of_formula a) ^ " ^ " ^ (string_of_formula b) ^ ")"
        | LOr (a, b) -> "(" ^ (string_of_formula a) ^ " v " ^ (string_of_formula b) ^ ")"
        | LNot a -> "~" ^ (string_of_formula a)

let rec string_of_evaluation v =
    match v with
        | (var, value) :: tl -> (String.make 1 var) ^
                                " = " ^
                                (string_of_bool value) ^
                                ", " ^
                                (string_of_evaluation tl)
        | [] -> ""

let rec eval p v =
    match p with
        | LTrue -> true
        | LFalse -> false
        | LVariable c -> v c
        | LNot a -> not @@ eval a v
        | LAnd (a, b) -> (&&) (eval a v) (eval b v)
        | LOr (a, b) -> (||) (eval a v) (eval b v)

let vars p =
    let rec aux s =
        match s with
            | LVariable c -> [c]
            | LNot a -> aux a
            | LAnd (a, b) -> (aux a) @ (aux b)
            | LOr (a, b) ->  (aux a) @ (aux b)
            | _ -> []
    in List.sort_uniq compare @@ aux p

let rec check_values ff v vars acc =
    match vars with
        | [] -> if ff v then (true, None) else (false, Some acc)
        | vh :: vt ->
                let v' t q = if q = vh then t else v q in
                let a = check_values ff (v' true)  vt ((vh, true)::acc) in
                let b = check_values ff (v' false) vt ((vh, false)::acc) in
                match (a, b) with
                    | ((true, _), (true, _)) -> (true, None)
                    | ((false, r), _) -> (false, r)
                    | (_, (false, r)) -> (false, r)

let is_tautology p =
    check_values (eval p) (fun p -> false) (vars p) []

let not_formula p =
    match p with
        | LAnd (a, b) -> LOr ((LNot a), (LNot b))
        | LOr (a, b) -> LAnd ((LNot a), (LNot b))
        | LNot a -> a
        | LTrue -> LFalse
        | LFalse -> LTrue
        | _ -> LNot p

let rec to_nnf p =
    match p with
        | LNot (LVariable _) -> p
        | LNot a -> to_nnf (not_formula a)
        | LAnd (a, b) -> LAnd (to_nnf a, to_nnf b)
        | LOr (a, b) -> LOr (to_nnf a, to_nnf b)
        | _ -> p

let to_cnf p =
    let rec aux p =
        match p with
            | LOr (a, LAnd (b, c)) -> LAnd (aux (LOr (a, b)), aux (LOr (a, c)))
            | LOr (LAnd (a, b), c) -> LAnd (aux (LOr (a, c)), aux (LOr (b, c)))
            | LOr (a, b) -> LOr (aux a, aux b)
            | LAnd (a, b) -> LAnd (aux a, aux b)
            | _ -> p
    in aux @@ to_nnf p

let to_dnf p =
    let rec aux p =
        match p with
            | LAnd (a, LOr (b, c)) -> LOr (aux (LAnd (a, b)), aux (LAnd (a, c)))
            | LAnd (LOr (a, b), c) -> LOr (aux (LAnd (a, c)), aux (LAnd (b, c)))
            | LAnd (a, b) -> LAnd (aux a, aux b)
            | LOr (a, b) -> LOr (aux a, aux b)
            | _ -> p
    in aux @@ to_nnf p

let tests () =
    let p = LNot (LAnd ( LVariable 'a', LOr( LVariable 'b', LVariable 'c' ))) in
    let t = LOr ( LVariable 'p', LNot ( LVariable 'p' ) ) in
    let check a b t = if t then a = b else t in
    let check_p p1 p2 s =
        if s then
            let p = LAnd( LOr (LNot p1, p2), LOr( LNot p2, p1 ) ) in
            fst ( is_tautology p )
        else false
    in
        true
    |> check (fst (is_tautology t)) true
    |> check (fst (is_tautology p)) false
    |> check_p p (to_nnf p)
    |> check_p p (to_cnf p)
    |> check_p p (to_dnf p)
