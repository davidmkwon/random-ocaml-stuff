(* references to memory locations *)
let counter = ref 0
let inc_counter = fun () ->
  counter := !counter + 1;
  !counter

(* note the differences between these two functions: the first returns a closure
 * that keeps the memory location [counter] in the body, so that every time it
 * is called the same memory location is updated. in the second function, a new
 * memory location is created each time the function is called *)
let inc_counter_hidden =
  let counter = ref 0 in
  fun () ->
    counter := !counter + 1;
    !counter

let inc_counter_hidden_wrong =
  fun () ->
    let counter = ref 0 in
    counter := !counter + 1;
    !counter

(* we can use references to get rid of "rec" keyword functions *)
let fib0 = ref (fun x -> x)
let fib n =
  if n <= 0 then 1
  else (!fib0) (n - 1) + (!fib0) (n - 2)

let () = fib0 := fib
let fifth_fib = fib 5

(* records with mutable fields
 * update the mutable field with [<-] syntax
 *
 * note: refs are actually just mutable record fields:
 *  [type 'a ref = { mutable contents: 'a;}]
 * *)
type point =
  { x: int
  ; y: int
  ; mutable c: string (* [mutable] is property, not type *)
  }

let p1 = {x=0; y=1; c="black"}
let () = p1.c <- "blue"

(* mutable stack ds *)
module type MutableStack =
  sig
    type 'a t

    val empty : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val peek : 'a t -> 'a
    val pop : 'a t -> unit
  end

module MutableRecordStack : MutableStack = struct
  type 'a node =
    { value: 'a
    ; mutable next: 'a node option
    }

  type 'a t = {mutable top: 'a node option}

  let empty () = {top=None}

  let push ele s =
    s.top <- Some {value=ele; next=None}

  let peek s =
    match s.top with
    | None -> raise (Failure "bad")
    | Some node -> node.value

  let pop s =
    match s.top with
    | None -> raise (Failure "bad")
    | Some node -> s.top <- node.next
end

(* arrays: fixed length, mutable sequences with constant time read/write *)
let arr = [|0;1;2;|]
let () = arr.(0) <- 3;; (* change value of index 0 *)

for x=0 to (Array.length arr - 1) do
  print_int arr.(x);
done;

for y=(Array.length arr - 1) downto 0 do
  print_int arr.(y);
done;
