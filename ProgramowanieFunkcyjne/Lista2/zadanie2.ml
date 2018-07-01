open Core_bench

let rec func_orig n =
    if n = 0 then 0
    else func_orig (n-1) * 2 + 1

let rec func_tail ?(a=0) n =
    if n = 0 then
        a
    else
        func_tail (n-1) ~a:(2*a+1)

let (^^) a n = truncate ((float_of_int a) ** (float_of_int n))

let func_closed n =
    if n = 0 then 0
    else (2^^n)-1

let rec forEach f l =
    match l with
        | [] -> ()
        | hd :: tl -> let _ = f hd in forEach f tl

let tests () =
    let info n a b x =
        let ok_fail b = if b then "ok" else "fail" in
        Printf.printf "%d\t%s\t\t%s\t\t%d\n" n (ok_fail (a=x)) (ok_fail (b=x)) x
    in
        print_string "#n\tfunc_tail\tfunc_closed\tf(x)\n";
        forEach
            (fun n -> info n (func_tail n) (func_closed n) (func_orig n))
            [1; 6; 10; 21; 44; 65; 99; 123]

let benches () =
    let n = 100000 in
        Printf.printf "\nBenchmarks for n=%d\n" n;
        Bench.bench [
            Bench.Test.create ~name:"Original func." (fun () -> ignore (func_orig n) );
            Bench.Test.create ~name:"Tail-rec func." (fun () -> ignore (func_tail n) );
            Bench.Test.create ~name:"Closed-form func." (fun () -> ignore (func_closed n) )
        ]

let () =
    tests ();
    benches ()
