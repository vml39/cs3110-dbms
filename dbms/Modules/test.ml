open OUnit2
open Computation
open Queries
open Datardwt


let data_read_write_tests = [
]

let queries_tests = [

]

let computatoin_tests = [

]



let suite =
  "test suite for dbms"  >::: List.flatten [
    data_read_write_tests;
    queries_tests;
    computatoin_tests;
  ]

let _ = run_test_tt_main suite