type 'a lstream = LStream of 'a * (unit -> 'a lstream)
type 'a lstream2 = LStream2 of 'a * 'a lstream2 Lazy.t

let sval (LStream(a, _)) = a
let sval_2 (LStream2(a, _)) = a
let snext_2 (LStream(_, f)) = f ()
let snext_2 (LStream2(_, lazy b)) = b
let rec sdrop s n =
    if n = 0 then s
    else let (LStream(_, f)) = s in sdrop (f ()) (n-1)
let rec sdrop_2 s n =
    if n = 0 then s
    else let (LStream2(_, lazy b)) = s in sdrop_2 b (n-1)

let rec leibniz ?(n=1.0) ?(i=2) () =
    let aux i =
        if i mod 2 = 0 then
            -1.0 /. (2.0 *. (float_of_int i) -. 1.0)
        else
            1.0 /. (2.0 *. (float_of_int i) -. 1.0)
    in
        LStream (4. *. n, fun () -> leibniz ~n:(n +. aux i) ~i:(i+1) ())

let rec smap3 (LStream(a, f)) t =
    let LStream (a2, f2) = f () in
    let LStream (a3, f3) = f2 () in
    LStream ( t a a2 a3, fun () -> smap3 (f3 ()) t )

let euler s =
    let aux a b c =
        c -. (b -. c) ** 2. /. (a -. 2. *. b +. c )
    in smap3 s aux

let faster_pi () =
    euler @@ leibniz ()

let rec leibniz_2 ?(n=1.0) ?(i=2) () =
    let aux i =
        if i mod 2 = 0 then
            -1.0 /. (2.0 *. (float_of_int i) -. 1.0)
        else
            1.0 /. (2.0 *. (float_of_int i) -. 1.0)
    in
        LStream2 ( 4. *. n, lazy( leibniz_2 ~n:(n +. aux i) ~i:(i+1) () ) )

let rec smap3_2 (LStream2(a, lazy b)) t =
    let LStream2 (a2, lazy b2) = b in
    let LStream2 (a3, lazy b3) = b2 in
    LStream2 ( t a a2 a3, lazy ( smap3_2 b3 t ) )

let euler_2 s =
    let aux a b c =
        c -. (b -. c) ** 2. /. (a -. 2. *. b +. c )
    in smap3_2 s aux

let faster_pi_2 () =
    euler_2 @@ leibniz_2 ()
