(* Lwt exposes idea of promises: a promise can be thought of as a box that will
 * eventually contain the result of some computation. Moreover, promises have
 * write-once semantics--that is, the value they hold can be changed only once.
 *
 * Once a promise is rejected (computation fails) or resolved (computation
 * succeeds), it may never change.
 *
 * We can think of an additional abstraction called the resolver as a component
 * that exists for every promise. The resolver is essentially a promise, but it
 * is only used by the OS libraries. The promise, on the other hand, is exposed
 * to the client. Moreover, the resolver is allowed to be written to (only
 * once), while the promise can only be read.
 *
 * taken from the book:
 * *)
(** A signature for Lwt-style promises, with better names *)
module type Promise = sig

  type 'a state = Pending | Resolved of 'a | Rejected of exn
  type 'a promise
  type 'a resolver

  (** [make ()] is a new promise and resolver. The promise is pending. *)
  val make : unit -> 'a promise * 'a resolver

  (** [return x] is a new promise that is already resolved with value [x]. *)
  val return : 'a -> 'a promise

  (** [state p] is the state of the promise *)
  val state : 'a promise -> 'a state

  (** [resolve r x] resolves the promise [p] associated with [r]
      with value [x], meaning that [state p] will become
      [Resolved x].
      Requires:  [p] is pending. *)
  val resolve : 'a resolver -> 'a -> unit

  (** [reject r x] rejects the promise [p] associated with [r]
      with exception [x], meaning that [state p] will become
      [Rejected x].
      Requires:  [p] is pending. *)
  val reject : 'a resolver -> exn -> unit

end

module PromiseImpl : Promise = struct

  type 'a state = Pending | Resolved of 'a | Rejected of exn
  type 'a promise = 'a state ref
  type 'a resolver = 'a promise

  (* this ensures that a promise is only written to once *)
  let write_once p s =
    if !p = Pending
    then p := s
    else invalid_arg "cannot write twice"

  let make () =
    let p = ref Pending in
    p, p

  let return x = ref (Resolved x)
  let state p = !p
  let resolve r x = write_once r (Resolved x)
  let reject r x = write_once r (Rejected x)

end

(* Lwt exposes a conceptually similar api of promises: *)
open Lwt

let (promise: int Lwt.t), resolver = Lwt.wait () (* same as make() *)
(* example: resolve a promise
# Lwt.state p;;
- : int Lwt.state = Lwt.Sleep

# Lwt.wakeup r 42;;
- : unit = ()

# Lwt.state p;;
- : int Lwt.state = Lwt.Return 42

# Lwt.wakeup r 42;;
Exception: Invalid_argument "Lwt.wakeup".
*)
let () = ignore promise

open Lwt_io
let stdin_promise = read_line stdin
(* type some text*)
let _ = Lwt.state stdin_promise (* will return [Lwt.Return (the stdin)] *)

(* callbacks: functions that are called when a promise is resolved *)
let print_the_string s = Lwt_io.printf "string: %s\n" s
let promise = read_line stdin
let () = ignore promise

(* register [print_the_string] as callback for [promise] *)
let () = ignore (Lwt.bind promise print_the_string)

(* [bind] does the following: when [promise] is resolved, it will execute the
 * callback function. the callback function will return another promise, of type
 * 'b Lwt.t. the [bind] function itself returns another promise of type 'b
 * Lwt.t, and this promise's value is set to the value of the first promise of
 * type 'b Lwt.t. this is done so that binds can be chained *)
let promise =
  Lwt.bind (read_line stdin) (fun s1 ->
      Lwt.bind (read_line stdin) (fun s2 ->
          Lwt_io.printf "string: %s\n" (s1^s2)))
let () = ignore promise

(* another way to write the above is with [>>=] operator: *)
let promise =
  read_line stdin >>= fun s1 ->
  read_line stdin >>= fun s2 ->
  Lwt_io.printf "string: %s\n" (s1^s2)
let () = ignore promise

(* YET another way to write the above is with [>>=] operator (i think this is
 * more idiomatic: *)
let promise =
  let%lwt s1 = read_line stdin in
  let%lwt s2 = read_line stdin in
  Lwt_io.printf "string: %s\n" (s1^s2)

let _ = Lwt_main.run promise
