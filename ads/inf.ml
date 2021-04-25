(* infinite values *)
let rec ones = 1::ones

(* infinite data type: stream
 * this works because functions are values--so even thought the function returns
 * the value to the next value in the stream (which continues to infinity), the
 * function's value isn't computed until it needs to be.
 *
 * we use a function that can take any value (and hence we use unit) and returns
 * the next value in the stream. this function is called a "thunk" apparently
 * *)
type 'a stream =
  Cons of 'a * (unit -> 'a stream)

let rec from n = Cons (n, (fun () -> from (n + 1)))
let naturals = from 1

let hd (Cons (hd, _)) = hd
let tl (Cons (_, tl)) = tl ()
let rec take n (Cons (hd, tl)) =
  if n = 0 then []
  else hd::take (n - 1) (tl ())

let rec square (Cons (hd, tl)) =
  Cons (hd * hd, (fun () -> square (tl ())))

let rec map f (Cons (hd, tl)) =
  Cons (f hd, (fun () -> map f (tl ())))

let rec map2 f (Cons (hd1, tl1)) (Cons (hd2, tl2)) =
  Cons (f hd1 hd2, (fun () -> map2 f (tl1 ()) (tl2 ())))

let sum s1 s2 =
  map2 (+) s1 s2

(* note that this takes too long: every time next fib value is computed, we need
 * to recompute two diff fib streams AKA there is no memoization of previously
 * computed values *)
let rec fibs =
  Cons (1, fun () ->
      Cons (1, fun () ->
          sum fibs (tl fibs)))

let first_ten_fib = take 10 fibs

(* the keyword [lazy] creates a type Lazy.t when you doing [lazy e] such that e
 * is not evaluated yet. e only becomes evaluated when you [force] it.
 *
 * Lazy.t types will also memoize the computed value [e] if the type has
 * previously been forced. so [e] will never be computed twice.
 * *)
let first_ten_fib_lazy = lazy (take 10 fibs)
let first_ten_fib_lazy_forced = Lazy.force first_ten_fib_lazy
(* this is computed instantly, because value of expression was memoized *)
let first_ten_fib_lazy_forced_immediate = Lazy.force first_ten_fib_lazy

(* infinite stream data structure with lazy values. this DS will be much more
 * efficient than the previous structure with thunks because it memoizes *)
type 'a lazy_stream =
  Cons of 'a * 'a lazy_stream Lazy.t
