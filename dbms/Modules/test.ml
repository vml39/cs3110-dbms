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
    "'I'm (Hungry     right,now); Dawg' is Select ['I'm';'Hungry;'right';'now']"
    (Select ["I'm";"Hungry";"right";"now"])
    "I'm (Hungry     right,now); Dawg";
]

(******************************************************************************)

let select_test name expected s = 
  "Select test: " ^ name >:: (fun _ -> 
    assert_equal expected (select s))  

let select_table_test name expected s =
  "Select table test: " ^ name >:: (fun _ -> 
      assert_equal expected (select_table s))

let select_fields_test name expected s =
  "Select fields test: " ^ name >:: (fun _ -> 
      assert_equal expected (select_fields [] s))

let malformed_table_test name s =
  "Malformed select table test: " ^ name >:: (fun _ -> 
      assert_raises Malformed (fun () -> (select_table s)))

let malformed_fields_test name s = 
  "Malformed select fields test: " ^ name >:: (fun _ ->
      assert_raises Malformed (fun () -> (select_fields [] s)))

let computation_tests = [
  select_table_test "get tablename" "animals" ["*"; "FROM"; "animals"];
  malformed_table_test "no FROM keyword" ["*"];
  malformed_table_test "no table called after FROM" ["*"; "FROM"];
  malformed_table_test "lowercase keyword from" ["*"; "from"; "animals"];
  select_fields_test "select all" ["*"] ["*"; "FROM"; "animals"];
  select_fields_test "get multiple fields" ["dog"; "cat"; "fish"] 
    ["dogs"; "cat"; "fish"; "FROM"; "animals"];
  malformed_fields_test "no fields" ["FROM"; "tablename"];
  malformed_fields_test "no FROM keyword" ["*"];
  malformed_fields_test "lowercase keyword from" ["dogs"; "from"; "animals"];
]

(******************************************************************************)

let suite =
  "test suite for dbms"  >::: List.flatten [
    data_read_write_tests;
    queries_tests;
    computation_tests;
  ]

let _ = run_test_tt_main suite