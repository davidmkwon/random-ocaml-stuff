open Core
open Async

(* runs computation after some amount delay of time--order of execution will
 * follow order of execution *)
module type Delayer_intf = sig
  type t
  val create : Time.Span.t -> t
  val schedule : t -> (unit -> 'a Deferred.t) -> 'a Deferred.t
end

module Delayer : Delayer_intf = struct
  type t =
    { delay: Time.Span.t
    ; queue: (unit -> unit) Queue.t
    }

  let create time = { delay=time; queue=Queue.create ();}

  (* no memory leaks here because Async makes deferred optimization similar to
   * tail-call optim. for functions *)
  let schedule t thunk =
    let ivar = Ivar.create () in
    Queue.enqueue t.queue (fun () ->
        upon (thunk ()) (fun x -> Ivar.fill ivar x));
    upon (after t.delay) (fun () ->
        let job = Queue.dequeue_exn t.queue in
        job ());
    Ivar.read ivar
end

(* custom async bind using ivar *)
let bind' d f =
  let i = Ivar.create () in
  upon d (fun x -> upon (f x) (fun y -> Ivar.fill i y));
  Ivar.read i

(* handling errors in async functions *)
let maybe_raise =
  let should_fail = ref false in
  fun () ->
    should_fail := not !should_fail;
    after (Time.Span.of_sec 0.5)
    >>= fun () ->
    if !should_fail then raise Exit else return ()

let handle_error () =
  try_with (fun () -> maybe_raise ())
  >>| function
  | Ok () -> "nice"
  | Error _ -> "not nice"

let () =
  upon (handle_error ()) (fun msg -> printf "%s\n" msg);
  never_returns (Scheduler.go ())
