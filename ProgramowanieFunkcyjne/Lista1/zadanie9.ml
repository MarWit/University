let int_stream n = n;;

let pow a b = truncate ( (float_of_int a) ** (float_of_int b) );;
let rec string_stream m = fun n -> if m == 0 then [String.make 1 (Char.chr( (n mod 26) + 97 ))] else [String.make 1 (Char.chr( (n / (pow 26 m) ) + 97 ))]@(string_stream (m-1) (n mod (pow 26 m)));;
let string5 n = String.concat "" (List.tl (string_stream 5 n));;

let hd s = s 0;;
let tl s = fun n -> s (n+1);;

let add s c = fun n -> (s n) + c;;
let map s f = fun n -> f (s n);;
let map2 s1 s2 f = fun n -> f (s1 n) (s2 n);;

let replace s i a = fun n -> if n mod i == 0 then a else s n;;
let take s i = fun n -> s (n*i);;
let rec fold s f a = fun n -> if n == 0 then f a (s 0) else f ((fold s f a) (n-1)) (s n);;
let rec tabulate s ?(a=0) b = if a == b then [s b] else [s a]@(tabulate s ~a:(a+1) b);;

let sum n = (fold int_stream (fun a b -> a + b) 0) n;;
let range = tabulate int_stream;;

