module type Stack = sig
  type 'a t (* idiomatic OCaml to name the type 't' because module type name
               should already give indicator on what it is *)
  val empty : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a
  val pop : 'a t -> 'a t
end

(* ListStack structure matches Stack module type signature
 * the representation type of 'a stack is with list *)
module ListStack : Stack = struct
  type 'a t = 'a list

  let empty = []
  let is_empty s = (s = [])
  let push x s = x::s
  let peek = function
    | [] -> failwith "Empty"
    | hd::_ -> hd
  let pop = function
    | [] -> failwith "Empty"
    | _::tl -> tl
end

(* MyStackImpl simply implements a stack using a its own data structure, however
 * this is not yet sealed to the Stack module type *)
module MyStackImpl = struct
  type 'a t =
    | Empty
    | Entry of 'a * 'a t

  let empty = Empty
  let is_empty s = (s = Empty)
  let push x s = Entry (x, s)
  let peek = function
    | Empty -> failwith "Empty"
    | Entry (hd,_) -> hd
  let pop = function
    | Empty -> failwith "Empty"
    | Entry (_, s) -> s
end

(* MyStack structure matches Stack module type signature using list
 * the representation type of 'a stack is with custom stack type *)
module MyStack : Stack = MyStackImpl


(* When a third party person wants to add functions to a library module, they
 * don't have the source code to edit. The include statement lets them include
 * all the values defined in the module/module type *)
module MyStackExtended = struct
  (* The "include" statement is equivalent to redefining all the values in the
   * module that is being implemented. So it's not like "open" where we bring
   * values into scope, but rather we are literally basically redefining them
   * here *)
  include MyStackImpl
  (* NOTICE how we have access to the constructors Empty and Entry because we
   * include MyStackImpl, which has not yet been sealed. If we included MyStack
   * instead, then type 'a t would be ABSTRACT and thus we wouldn't have access
   * to the underlying implementation *)
  let return_unit = function
    | Empty -> ()
    | Entry (_, _) -> ()
  let call_empty = empty
end

(* sometimes when you want to write a function that operates on the values
 * inside multiple modules, you need to reimplement it, making for verbose and
 * repetitive code. solution: functors. functors are "functions" that map
 * modules to other modules, and can add things to the module *)

(* example: we want to test our stack implementations, but writing test code for
 * each implementation can be repetitive *)
let () = assert (ListStack.(empty |> push 1 |> peek) = 1)
let () = assert (MyStack.(empty |> push 1 |> peek) = 1)

(* solution: use functor for testing *)
module StackTester (S:Stack) = struct
  let () = assert (S.(empty |> push 1 |> peek) = 1)
end

(* stack tester takes as input a module that matches stack, and returns as
 * output a new module with the tests part of the module definitions *)
module ListStackTester = StackTester (ListStack)
module MyStackTester = StackTester (MyStack)

(* we can also use SHARING CONSTRAINTS to refine a signature such that a
 * specific equation must hold with respect to the abstract type
 *
 * in this example, we enforce/constrait the module so that the type 'a t is of
 * type 'a list. *)
module ListStackEnforced : (Stack with type 'a t = 'a list) = struct
  type 'a t = 'a list

  let empty = []
  let is_empty s = (s = [])
  let push x s = x::s
  let peek = function
    | [] -> failwith "Empty"
    | hd::_ -> hd
  let pop = function
    | [] -> failwith "Empty"
    | _::tl -> tl
end
