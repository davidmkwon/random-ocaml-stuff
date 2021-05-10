(* some memoization examples *)
open Base

(* fibONACCI *)
let rec fib_bad n =
  if n <= 1 then 1
  else fib_bad (n - 1) + fib_bad (n - 2)

let fib_mem n =
  let memo = Array.create ~len:(n+1) (-1) in
  let rec fib_mem n =
    if n <= 1 then 1
    else if Array.get memo n <> (-1) then Array.get memo n
    else
      let res = fib_mem (n - 1) + fib_mem (n - 2) in
      Array.set memo n res;
      res
  in
  fib_mem n

(* max weight independent set of tree *)
module TreeKey = struct
  type t =
    | Leaf
    | Node of int * t * t
  [@@deriving sexp, compare, hash]
end
include TreeKey

let rec party_bad t =
  max (party_bad_in t) (party_bad_out t)
and party_bad_in = function
  | Leaf -> 0
  | Node (v, l, r) -> v + (party_bad_out l) + (party_bad_out r)
and party_bad_out = function
  | Leaf -> 0
  | Node (_, l, r) -> (party_bad l) + (party_bad r)

let rec party t =
  let ht = Hashtbl.create (module TreeKey) in
  max (party_in ht t) (party_out ht t)
and party_in ht = function
  | Leaf -> 0
  | Node (v, l, r) as n ->
    match Hashtbl.find ht n with
    | None -> v + (party_out ht l) + (party_out ht r)
    | Some x -> x
and party_out ht = function
  | Leaf -> 0
  | Node (_, l, r) as n ->
    match Hashtbl.find ht n with
    | None -> (party l) + (party r)
    | Some x -> x
