open OUnit2
open Computation
open Queries
open Datardwt


let data_read_write_tests = [
]

(* [make_query_test name expected s] constructs an OUnit test named 
   [name] that asserts the quality of [expected] of [s] applied to 
   query.parse*)
let make_query_test name expected s =
  "query Test: " ^ name >:: (fun _ -> 
      assert_equal expected (parse s))

(* [malformed_query_test name s] constructs an OUnit test named [name] that 
   asserts [s] applied to [parse] raises a query.Malformed exception*)
let malformed_query_test name s =
  "Malformed query Test: " ^ name >:: (fun _ -> 
      assert_raises Malformed (fun () ->  (parse s)))

(* [malformed_query_test name s] constructs an OUnit test named [name] that 
   asserts [s] applied to query.parse raises a query.Empty exception*)
let empty_query_test name s =
  "Empty query Test: " ^ name >:: (fun _ -> 
      assert_raises Empty (fun () ->  (parse s)))

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