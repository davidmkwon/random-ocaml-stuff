(** [append lst1 lst2] combines the two lists *)
let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h::t -> h::(append t lst2)

(** [length_2 lst] is whether lst has length 2 *)
let length_2 (lst: int list)  =
  match lst with
  | _::_::[] -> true
  | _ -> false

(** [sum_tr lst] is the sum of the list using tail recursion *)
let sum_tr lst =
  let rec h_sum_tr acc lst =
    match lst with
      | [] -> acc
      | h::t -> h_sum_tr (acc + h) t
  in
  h_sum_tr 0 lst

(* tests *)
let () = assert ((append [1;2;3] [4;5;6] ) = [1;2;3;4;5;6])
let () = assert ((length_2 [1;2;3]) = false)
let () = assert ((sum_tr [1;2;3]) = 6)

(* day variant *)
type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat

(* records = struct *)
type person = {name: string; age: int}
let david = {name="david"; age=19}

(* you can pattern match function arguments *)
let first (x, _, _) = x
let second (_, y, _) = y
let third (_, _, z) = z

(* dictionary implemented via association list (list of pairs) *)
let d = [("rectangle", 4); ("triangle", 3); ("octagon", 8);]
let insert k v d =
  (k, v)::d
let rec get k = function
  | [] -> None
  | (k',v')::t -> if k=k' then Some v' else get k t

(* you can have synonyms for types *)
type point = float * float

(* variant type = algebraic data type
   the constructors can carry additional info.
   "algebraic" = 'algebra' because variant types are
   a [sum] type of potentially [product] types *)
type shape =
  | Point of point
  | Circle of float * point
  | Rectangle of point * point

let circle1 = Circle (4.0, (3.1, 2.5))
let area shape =
  match shape with
  | Point _ -> 0.0
  | Circle (r,_) -> 3.14 *. r *. r
  | Rectangle ((x,y),(x',y')) -> (x' -. x) *. (y' -. y)

type string_or_int = String of string | Int of int

(* can distinguish between multiple constructors that carry same type *)
type left_right = Left of int | Right of int

let double_left = function
  | Left i -> 2 * i
  | Right i -> i

(* recursive variants *)
type rec_list = Nil | Cons of int * rec_list

(* variants can be parametrized over types, aka [type constructors] *)
(* type constructors map types -> types, like functions map vals -> vals *)
type 'a p_rec_list = Nil | Cons of 'a * 'a p_rec_list

(* parametric polymorphism (above is example of this too) *)
type ('a, 'b) pair = 'a * 'b

(* stdlib's option type is really defined as:
   type 'a option = None | Some of 'a *)

(* exception definition *)
exception Eint of int
exception Estring of string

(* exception handling, an expression must be between try and with *)
let handle_exception =
  try raise (Estring "error bruh") with
  | Eint n -> print_int n
  | Estring msg -> print_string msg

exception A
exception B
let raise_exception_A =
  let _ = raise A in raise B

let _ =
  match List.hd [] with
  | [] -> "empty"
  | _ -> "non empty"
  | exception (Failure e) -> e

(* binary tree *)
type 'a bin_tree = Leaf | Node of 'a * 'a bin_tree * 'a bin_tree

(* also binary tree using recursive definition with records *)
type 'a bin_tree_record = Leaf | Node of 'a node
and 'a node = {
  data: 'a;
  left: 'a bin_tree_record;
  right: 'a bin_tree_record
}

(* [mem bin_tree_record ele] returns whether ele is present in
   [bin_tree_record] *)
let rec mem tree ele =
  match tree with
  | Leaf -> false
  | Node {data;left;right} -> ele=data || (mem left ele) || (mem right ele)
