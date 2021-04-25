open OUnit2
open Data

let tests = "test suite">::: [
    "sum list 1" >:: (fun _ -> assert_equal 2 (sum_tr [1;1]));
    "sum list 2" >:: (fun _ -> assert_equal 7 (sum_tr [2;5]));
]

let _ = run_test_tt_main tests
