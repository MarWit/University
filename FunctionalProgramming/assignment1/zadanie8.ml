(* ???????? *)

let compose f g x = f (g x);;
let rec iterate f n = if n == 1 then f else compose f (iterate f (n-1));;

let mult a b = (iterate ((+) a) b) 0;;
let pow a n = (iterate ((mult) a) n) 1;;
let (^^) = pow;;
