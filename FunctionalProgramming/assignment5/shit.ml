let rec fix f x = f (fix f) x

let factf = fun f n -> if n = 0 then 1 else n * (f (n-1))
let fact =
    let f = ref @@ (fun x -> x) in
    f := (fun n -> if n <> 1 then n * (!f (n-1)) else 1);
    !f

let fix2 f =
    let aux = ref @@ (fun x -> x) in
    aux := (fun x -> f !aux x);
    !aux

let tests () =
    let check a b t =
        if t then a = b else t
    in
        true
    |> check (fix factf 10) 3628800
    |> check (fix factf 10) (fact 10)
    |> check (fix factf 10) (fix2 factf 10)
