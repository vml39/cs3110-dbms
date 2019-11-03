open OUnit2
open Computation
open Query
open Datardwt

(******************************************************************************)

let data_read_write_tests = [
]

(******************************************************************************)

(* [make_query_test name expected s] constructs an OUnit test named 
   [name] that asserts the quality of [expected] of [s] applied to 
   query.parse*)
let query_test name expected s =
  "Query Test: " ^ name >:: (fun _ -> 
      assert_equal expected (parse s))

(* [malformed_query_test name s] constructs an OUnit test named [name] that 
   asserts [s] applied to [parse] raises a query.Malformed exception*)
let malformed_test name s =
  "Malformed query Test: " ^ name >:: (fun _ -> 
      assert_raises Malformed (fun () ->  (parse s)))

(* [malformed_query_test name s] constructs an OUnit test named [name] that 
   asserts [s] applied to query.parse raises a query.Empty exception*)
let empty_test name s =
  "Empty query Test: " ^ name >:: (fun _ -> 
      assert_raises Empty (fun () ->  (parse s)))

let queries_tests = [
  query_test "'SELECT a FROM alpha' is Select ['a';'FROM';'alpha']"
    (Select ["a";"FROM";"alpha"]) "SELECT a FROM alpha";
  query_test 
    "'SELECT (a,b,c) FROM (alpha)' is Select ['a';'b';'c';'FROM';'alpha']"
    (Select ["a";"b";"c"; "FROM"; "alpha"]) 
    "SELECT (a,b,c) FROM (alpha)";
  query_test 
    "'SELECT ( a,   b,c) FROM (alpha  )' is Select ['a';'b';'c';'FROM';'alpha']"
    (Select ["a";"b";"c"; "FROM"; "alpha"]) 
    "SELECT (a,b,c) FROM (alpha)";
  query_test
    "'I'm (Hungry     right,now); Hi' is Select ['I'm';'Hungry;'right';'now']"
    (Select ["I'm";"Hungry";"right";"now"])
    "I'm (Hungry     right,now); Hi";
]

(******************************************************************************)

let computation_tests = [

]

(******************************************************************************)

let suite =
  "test suite for dbms"  >::: List.flatten [
    data_read_write_tests;
    queries_tests;
    computation_tests;
  ]

let _ = run_test_tt_main suite