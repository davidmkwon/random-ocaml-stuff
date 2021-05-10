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

(* Writer Monad for ints: *)
module WriterInt = struct
  let inc x = x + 1
  let dec x = x - 1
  let (>>) f g x = x |> f |> g
  let id = inc >> dec

  let log (name: string) (f: int -> int) : int -> int * string =
    fun x -> (f x, Printf.sprintf "Called %s on %i" name x)

  let loggable (name: string) (f: int -> int) : int * string -> int * string =
    fun (x, s1) ->
      let (y, s2) = x |> log name f in
      (y, Printf.sprintf "%s | %s" s1 s2)

  let return (x: int) : int * string =
    (x, "")

  let (>>=) (v: int * string) (f: int -> int * string) : int * string =
    let (x, s) = v in
    let (y, s2) = f x in
    (y, Printf.sprintf "%s | %s" s s2)

  let loggable' (name: string) (f: int -> int) : int * string -> int * string =
    fun v ->
      v >>= log name f

  let inc' : int * string -> int * string =
    loggable' "inc" inc

  let dec' : int * string -> int * string =
    loggable' "dec" dec

  let id' = inc' >> dec'
end

(* general Writer Monad: *)
module WriterMonad : Monad = struct
  type 'a t = 'a * string

  let return x = (x, "")

  let (>>=) x f =
    let (v, s) = x in
    let (v2, s2) = f v in
    (v2, s ^ s2)
end

(*
 * Some monad rules:
 * [return x >>= f] is [f x]
 * [m >>= return] is [m]
 * [(m >>= f) >>= g)] is m >>= (fun x -> f x >>= g)
 *
 * or equivalently using compose (>=>):
 * [return >=> f] is [f]
 * [f >=> return] is [f]
 * [(f >=> g) >=> h] is [f >=> (g >=> h)]
 *)
