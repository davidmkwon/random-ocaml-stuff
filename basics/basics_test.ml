open OUnit2
open Basics

(* helper function to make test that OUnit2 expects *)
let make_inc_test name expected input =
    name >:: (fun _ -> assert_equal expected (inc input) ~printer:string_of_int)

(* tests *)
let tests = "test suite">::: [
    make_inc_test "inc 1" 3 1;
    make_inc_test "inc 1" 4 2;
]

(* execute test suite *)
let _ = run_test_tt_main tests
let _ = Printf.printf "%d\n" (fib_fast 3)
let _ = Printf.printf "hi"
let _ = assert_equal [3;4] [1;2]
