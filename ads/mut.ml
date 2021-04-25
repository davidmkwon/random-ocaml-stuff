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
