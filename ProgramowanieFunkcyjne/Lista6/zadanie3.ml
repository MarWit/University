let sprintf format = format (fun v -> v) ""

let (++) f g = fun x -> f ( g x )
let lit s f ss = f ( ss ^ s )
let eol f ss = f ( ss ^ "\n" )
let inr f ss n = f ( ss ^ string_of_int n )
let flt f ss n = f ( ss ^ string_of_float n )
let str f ss s = f ( ss ^ s )

let ala_ma n =
    (sprintf (lit "Ala ma " ++ inr ++ lit " kot" ++ str ++ lit "."))
    n (if n = 1 then "a" else if 1 < n && n < 5 then "y" else "ow")

let tests () =
    let check a b t =
        if t then a = b else t
    in
        true
    |> check (ala_ma 1) "Ala ma 1 kota."
    |> check (ala_ma 2) "Ala ma 2 koty."
    |> check (ala_ma 5) "Ala ma 5 kotow."
