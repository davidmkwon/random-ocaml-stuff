(* another example with a Set module type *)
module type Set = sig
  type 'a t

  val empty : 'a t
  val mem : 'a -> 'a t -> bool
  val add : 'a -> 'a t -> 'a t
  val elts : 'a t -> 'a list
end

(* Set interface implementation with list *)
module ListSetNoDup : Set = struct
  type 'a t = 'a list

  let empty = []
  let mem x s = List.fold_left (fun acc ele ->
      acc || (ele = x)) false s
  let add x s = if mem x s then s else x::s
  let elts s = s
end

(* Set interface implementation with list and duplicates *)
module ListSetDup : Set = struct
  type 'a t = 'a list

  let empty = []
  let mem x s =
    let op = fun acc ele -> acc || (ele = x) in
    List.fold_left op false s
  let add x s = x::s
  let elts s = s
end

(* functor that adds [add_all] function
 *
 * note the use of "include S", if we don't write this then this functor just
 * returns a module with the one definition of [add_all]. we want this functor
 * to return a "modified" Stack module with this extra definition, so we
 * simulate that by adding an include S *)
module AddAll (S:Set) = struct
  include S

  let add_all lst set =
    let op = (fun acc ele -> add ele acc) in
    List.fold_left op set lst
end

module ListSetNoDupExtended = AddAll (ListSetDup)

(* NOTE: to be reminded of/understand how functors can be used in practice,
 * consult
 * (https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/modules/stdlib_map.html)
 * or just look at stdlib's Map module *)
