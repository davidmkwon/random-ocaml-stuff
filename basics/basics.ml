(* requires: n >= 0 *)
let rec factorial n =
    if n = 0 then 1 else n * factorial (n - 1)

(** [inc num] prints num and is num + 2 *)
let inc x =
    print_int x;
    x + 2

(** [square x] is x squares *)
let square x = x * x

(** [rms x y] is the root mean squared of x and y *)
let rms x y =
    sqrt (Float.of_int ((+) (square x) (square y)) /. 2.0)

(** [fib n] is the nth fibonacci number. this runs in exponential time *)
let fib n' =
    let rec fib' n =
        (*if (n = 0 || n = 1) then 1*)
        if (n < 2) then 1
        else fib' (n - 1) + fib' (n - 2)
    in
    let n = n' - 1 in fib' n

(** [fib_fast n] is the nth fibonacci number. this runs in linear time *)
let fib_fast n' =
    let fib_fast' n =
        let rec h n pp p =
            if (n = 1) then p else h (n - 1) p (pp + p)
        in h n 0 1
    in
    let n = n' - 1 in fib_fast' n

(* tests *)
let () = assert (rms 4 4 = 4.0)
let () = assert (true || false)
let () = assert (fib 1 = 1)
let () = assert (fib 2 = 1)
let () = assert (fib 3 = 2)
let () = assert (fib 4 = 3)
