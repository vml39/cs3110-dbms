open OUnit2
open Computation
open Query
open Datardwt

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [pp_string s] pretty-prints string [s]. *)
let pp_query s = "\"" ^ s ^ "\""

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
    "'SELECT Im (Hungry  rite,now); Dawg' is Select ['Im';'Hungry;'rite';'now']"
    (Select ["Im";"Hungry";"rite";"now"])
    "SELECT Im (Hungry rite,now); Dawg";
]

(******************************************************************************)
let rec print_list = function 
  | [] -> ""
  | h::t -> h ^ " " ^ print_list t

let get_qry = function 
  | Select qry -> qry
  | _ -> failwith "unimplemented"

(* [select_test name expected s] constructs an OUnit test named 
   [name] that asserts the quality of [expected] of [s] applied to 
   Computation.select*)
let select_test name expected s = 
  "Select test: " ^ name >:: (fun _ -> 
      assert_equal expected (select s))  

(* [select_table_test name expected s] constructs an OUnit test named 
   [name] that asserts the quality of [expected] of [s] applied to 
   Computation.select_table*)
let select_table_test name expected s =
  "Select table test: " ^ name >:: (fun _ -> 
      assert_equal expected (select_table s))

(* [select_fields_test name expected s] constructs an OUnit test named 
   [name] that asserts the quality of [expected] of [s] applied to 
   Computation.select_fields*)
let select_fields_test name expected s =
  "Select fields test: " ^ name >:: (fun _ -> 
      assert_equal ~printer:(print_list) expected (select_fields [] s))

(* [malformed_table_test name s] constructs an OUnit test named 
   [name] that asserts [s] applied to Computation.select_table raises a 
   query.Malformed exception*)
let malformed_table_test name s =
  "Malformed select table test: " ^ name >:: (fun _ -> 
      assert_raises Malformed (fun () -> (select_table s)))

(* [malformed_fields_test name s] constructs an OUnit test named 
   [name] that asserts [s] applied to Computation.select_fields raises a 
   query.Malformed exception*)
let malformed_fields_test name s = 
  "Malformed select fields test: " ^ name >:: (fun _ ->
      assert_raises Malformed (fun () -> (select_fields [] s)))

let qry = parse "SELECT netid FROM students" |> get_qry
let qry' = parse "SELECT netid, name FROM students" |> get_qry
let qry'' = parse "SELECT * FROM students" |> get_qry
let schema = ["name"; "netid"; "gradyear"; "major"; "home"]
let fields = [false; true; false; false; false]
let fields' = [true; true; false; false; false]
let fields'' = [true; true; true; true; true ]
let namenetid = [
  ["Daniel Stabile"; "dis52"]; 
  ["Robert Morgowicz"; "rjm448"]; 
  ["Vivian Li"; "vml39"]
]
let students = [
  ["Daniel Stabile"; "dis52"; "2021"; "CS"; "Cascadilla Hall"];
  ["Robert Morgowicz"; "rjm448"; "2020"; "ECE"; "Cascadilla Hall"];
  ["Vivian Li"; "vml39"; "2020"; "IS"; "Collegetown"]
]

let computation_tests = [
  select_table_test "get tablename" "animals" ["*"; "FROM"; "animals"];
  malformed_table_test "no FROM keyword" ["*"];
  malformed_table_test "no table called after FROM" ["*"; "FROM"];
  malformed_table_test "lowercase keyword from" ["*"; "from"; "animals"];
  select_fields_test "select all" ["*"] ["*"; "FROM"; "animals"];
  select_fields_test "get multiple fields" ["dogs"; "cat"; "fish"]
    ["dogs"; "cat"; "fish"; "FROM"; "animals"];
  malformed_fields_test "no fields" ["FROM"; "tablename"];
  malformed_fields_test "no FROM keyword" ["*"];
  malformed_fields_test "lowercase keyword from" ["dogs"; "from"; "animals"];
  select_test "SELECT netid FROM students" 
    (schema, fields, [["dis52"]; ["rjm448"]; ["vml39"]]) qry;
  select_test "SELECT netid, name FROM students" 
    (schema, fields', namenetid) qry';
  select_test "SELECT * FROM students" (schema, fields'', students) qry''
]

(******************************************************************************)

let suite =
  "test suite for dbms"  >::: List.flatten [
    data_read_write_tests;
    queries_tests;
    computation_tests;
  ]

let _ = run_test_tt_main suite