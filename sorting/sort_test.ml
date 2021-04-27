open OUnit2
open Base

open Sort

(* list to string for ounit *)
let list_printer (lst:int list) =
  let rec rec_list_printer acc = function
    | [] -> acc
    | hd::tl -> rec_list_printer (Printf.sprintf "%s %d" acc hd) tl
  in
  rec_list_printer "" lst

(* unit test helper function *)
let make_sort_test name sort expected input =
  name >:: fun _ ->
    assert_equal expected (sort ~lst:input) ~printer:list_printer

(* unit tests *)
let tests = "test suite">::: [
    make_sort_test
      "selection sort 1" (selection_sort) [1;2;3] [3;1;2];
    make_sort_test
      "selection sort 2" (selection_sort) [1;2;3;4;5] [5;4;3;2;1];
    make_sort_test
      "selection sort 3" (selection_sort) [1;3;8] [8;1;3];
    make_sort_test
      "merge sort 1" (merge_sort) [1;3;8] [8;1;3];
    make_sort_test
      "merge sort 2" (merge_sort) [1;2;3;4;5] [5;4;3;2;1];
    make_sort_test
      "merge sort 3" (merge_sort) [0;1;2;5;6;9] [1;6;2;5;0;9];
    make_sort_test
      "quick sort 1" (quick_sort) [0;1;2;5;6;9] [1;6;2;5;0;9];
    make_sort_test
      "quick sort 2" (quick_sort) [0;1;2;5;6;9] [1;6;2;5;0;9];
    make_sort_test
      "quick sort 3" (quick_sort) [0;1;2;5;6;9] [1;6;2;5;0;9];
    make_sort_test
      "insertion sort 1" (insertion_sort) [0;1;2;5;6;9] [1;6;2;5;0;9];
    make_sort_test
      "insertion sort 2" (insertion_sort) [0;1;2;5;6;9] [1;6;2;5;0;9];
    make_sort_test
      "insertion sort 3" (insertion_sort) [0;1;2;5;6;9] [1;6;2;5;0;9];
  ]

(* execute test suite *)
let _ = run_test_tt_main tests
