(* just read
 * https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/ads/monads.html
 * for more info on monads. but essentially, a monad is a structure that has a
 * [return] and [bind] operator, and is used to chain computations.
*)
module type Monad = sig
  type 'a t

  val return: 'a -> 'a t

  (* [>>=] is bind*)
  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
end

(* Maybe Monad for ints: *)
module MaybeInt = struct
  let return (x: int) : int option =
    Some x

  let (>>=) (x: int option) (op: int -> int option) : (int option) =
    match x with
    | None -> None
    | Some v -> op v

  let (+) (x: int option) (y: int option) : (int option) =
    x >>= fun a ->
    y >>= fun b ->
    return (Stdlib.(+) a b)

  let (-) (x: int option) (y: int option) : (int option) =
    x >>= fun a ->
    y >>= fun b ->
    return (Stdlib.(-) a b)

  let (/) (x: int option) (y: int option) : (int option) =
    x >>= fun a ->
    y >>= fun b ->
    if b = 0 then None else return (Stdlib.(/) a b)
end

(* regular Maybe Monad: *)
module Maybe : Monad = struct
  type 'a t = 'a option

  let return x = Some x

  let (>>=) x f =
    match x with
    | None -> None
    | Some v -> f v
end
