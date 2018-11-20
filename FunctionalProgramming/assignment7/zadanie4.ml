let fresh =
    let x = ref 1 in
    let aux ?(a=None) s =
        ignore( match a with
            | None -> ()
            | Some n -> x := n
        );
        x := !x + 1;
        s ^ string_of_int (!x - 1)
    in aux

let reset n = ignore @@ fresh ~a:(Some n) ""

let tests () =
    let check a b t =
        if t then a = b else t
    in
        true
    |> check (fresh "a") "a1"
    |> check (fresh "a") "a2"
    |> check (fresh "a") "a3"
    |> check (reset 41) ()
    |> check (fresh "answer=") "answer=42"
