let check_matrix m =
    let aux acc r =
        if acc = -1 then -1
        else if acc = 0 then List.length r
        else if acc != (List.length r) then -1
        else acc
    in (List.fold_left aux 0 m) = (List.length m)

let col m n =
    List.map (fun v -> List.nth v n) m

let range n =
    let rec aux n acc =
        if n = 0 then 0 :: acc
        else aux (n-1) (n :: acc)
    in List.rev ( aux n [] )

let transpose m =
    List.fold_left (fun x y -> y::x) [] (List.map (col m) (range ((List.length m) - 1)))

let rec zip a b =
    match a, b with
        | (ahd :: atl, bhd :: btl) -> (ahd, bhd) :: zip atl btl
        | ([], []) -> []
        | _ -> failwith "List with different lenghts."

let zipf a b f = List.map f (zip a b)

(* Mnożenie macierzy przez wektor *)
let mult_vec a v = List.map ( fun av -> List.fold_left (+.) 0.0 (zipf av v (fun (x, y) -> x *. y ))) a

let matrix_mult a b =
    List.map (fun n -> mult_vec a (col b n)) (List.rev (range @@ (List.length a) - 1))
        |> transpose


let tests () =
    let id = [[1.;0.];[0.;1.]] in
    let check a b r = if r then a = b else r in
        true
    |>  check [[7.;10.]; [15.;22.]] (matrix_mult [[1.;2.];[3.;4.]] [[1.;2.];[3.;4.]])
    |>  check [[30.;24.;18.];[84.;69.;54.];[138.;114.;90.]] (matrix_mult [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]] [[9.;8.;7.];[6.;5.;4.];[3.;2.;1.]])
    |>  check id (matrix_mult id id)
    |>  check [[4.;2.];[2.;4.]] (matrix_mult [[4.;2.];[2.;4.]] id)


