open Ads
open Base

let () = Stdlib.Printf.printf "%d\n" (Memo.fib_mem 3)

type t = {
  foo: int;
  bar: int;
} [@@deriving sexp]

let t = {foo=3; bar=5}

let _ = Base.(t |> sexp_of_t |> Sexp.to_string |> Stdlib.print_endline)
