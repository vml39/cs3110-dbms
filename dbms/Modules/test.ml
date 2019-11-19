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

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts (List.flatten lst) ^ "]"

let pp_list_list' pp_elt lst =
  let lst' = snd lst in 
  let pp_elts lst' =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst'
  in "[" ^ pp_elts (List.flatten lst') ^ "]"

(** [pp_string s] pretty-prints string [s]. *)
let pp_query s = "\"" ^ s ^ "\""

(* QUERY TESTS ****************************************************************)

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

(* COMPUTATION TESTS **********************************************************)

let get_qry = function 
  | Select qry -> qry
  | Insert qry -> qry
  | _ -> failwith "unimplemented"

(* [select_test name expected s] constructs an OUnit test named 
   [name] that asserts the quality of [expected] of [s] applied to 
   Computation.select*)
let select_test name expected s = 
  "Select test: " ^ name >:: (fun _ -> 
      assert_equal ~printer:(pp_list_list' pp_query) expected (select s))  

(* [insert_test name expected s] constructs an OUnit test named 
   [name] that asserts the quality of [expected] of [s] applied to 
   Computation.insert*)
let insert_test name expected s = 
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
      assert_equal expected (select_fields [] s))

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
let qry_order = parse "SELECT * FROM students ORDER BY name" |> get_qry
let schema = ["name"; "netid"; "class"; "major"; "home"]
let fields = ["netid"]
let fields' = ["name"; "netid"]
let namenetid = [
  ["Daniel Stabile"; "dis52"]; 
  ["Robert Morgowicz"; "rjm448"]; 
  ["Vivian Li"; "vml39"];
  ["Test"; "t123"]
]
let students = [
  ["Daniel Stabile"; "dis52"; "2021"; "CS"; "Cascadilla Hall"];
  ["Robert Morgowicz"; "rjm448"; "2020"; "ECE"; "Cascadilla Hall"];
  ["Vivian Li"; "vml39"; "2020"; "IS"; "Collegetown"];
  ["Test"; "t123"; "2022"; "Government"; "North"]
]
let students_ordered = [
  ["Daniel Stabile"; "dis52"; "2021"; "CS"; "Cascadilla Hall"];
  ["Robert Morgowicz"; "rjm448"; "2020"; "ECE"; "Cascadilla Hall"];
  ["Test"; "t123"; "2022"; "Government"; "North"];
  ["Vivian Li"; "vml39"; "2020"; "IS"; "Collegetown"]
]

let select_tests = [
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
    (fields, [["dis52"]; ["rjm448"]; ["vml39"]; ["t123"]]) qry;
  select_test "SELECT netid, name FROM students" 
    (fields', namenetid) qry';
  select_test "SELECT * FROM students" (schema, students) qry'';
  select_test 
    "SELECT * FROM students ORDER BY name" (schema, students_ordered) qry_order
]

let ins_qry1 = parse "INSERT INTO students VALUES (Joe, jfs9, 1969, ECE, Collegetown)" |> get_qry
let post_ins1 = (*insert ins_qry1;*) parse "SELECT * FROM students" |> get_qry
let ins_qry2 = parse "INSERT INTO students (name, netid, major) VALUES (Joe, jfs9, ECE)" |> get_qry
let post_ins2 = (*insert ins_qry2;*) parse "SELECT * FROM students" |> get_qry

let students_up_1 = students @ [["Joe"; "jfs9"; "1969"; "ECE"; "Collegetown"]]
let students_up_2 = students @ [["Joe"; "jfs9"; ""; "ECE"; ""]]

let insert_tests = [
  select_test "INSERT full" (fields', students_up_1) post_ins1;
  select_test "INSERT partial" (fields', students_up_2) post_ins2;
]

let delete_tests = [

]

(* DATA READ WRITE TESTS ******************************************************)

(* [table_from_txt_test name expected s] constructs an OUnit test named 
   [name] that asserts the equality of [expected] of [s] applied to 
   DataRdWt.table_from_text*)
let table_from_txt_test name expected s = 
  "Table from Text test: " ^ name >:: (fun _ -> 
      assert_equal ~printer:(pp_list_list pp_query) 
        expected (table_from_txt s))

(* [schema_from_txt_test name expected s] constructs an OUnit test named 
   [name] that asserts the equality of [expected] of [s] applied to 
   DataRdWt.schema_from_text*)
let schema_from_txt_test name expected s = 
  "Schema from Text test: " ^ name >:: (fun _ -> 
      assert_equal expected (schema_from_txt s))

(* [str_equ_test name expected s] constructs an OUnit test named 
   [name] that asserts the equality of [expected] of [s] 
*)
let str_lst_eq_test name expected s = 
  "Schema from Text test: " ^ name >:: (fun _ -> 
      assert_equal ~printer:(pp_list pp_query) expected s)

(* [func_raises_test name expected f s] constructs an OUnit test named 
   [name] that asserts [f] applied to [s] raises [expected]
*)
let f_raises_test name exn f s = 
  "Schema from Text test: " ^ name >:: (fun _ -> 
      assert_raises exn (fun _ -> (f s)))

let fc = get_in_chan "students"
let ln1 = read_next_line fc
let ln2 = read_next_line fc
let fc2 = get_in_chan "students"

let schema2 = [
  ("students", ["name"; "netid"; "class"; "major"; "home"]);
  ("buildings", ["name"; "location"; "goodforstudying"])
]

let data_read_write_tests = [
  table_from_txt_test "students" students "students";
  schema_from_txt_test "example" schema2 ();
  str_lst_eq_test "ln1" (List.nth students_ordered 0) ln1;
  str_lst_eq_test "ln2" (List.nth students_ordered 1) ln2;
  str_lst_eq_test "ln3" (List.nth students_ordered 2) (read_next_line fc);
  str_lst_eq_test "ln4" (List.nth students_ordered 3) (read_next_line fc);
  f_raises_test "no ln 5" (End_of_file) read_next_line fc;
  (* Note Reverse Order *)
  f_raises_test "no ln 5" (End_of_file) read_next_line fc2;
  str_lst_eq_test "ln4 again" (List.nth students 3) (read_next_line fc2);
  str_lst_eq_test "ln3 again" (List.nth students 2) (read_next_line fc2);
  str_lst_eq_test "ln2 again" (List.nth students 1) (read_next_line fc2);
  str_lst_eq_test "ln1 again" (List.nth students 0) (read_next_line fc2);
]



(******************************************************************************)

let suite =
  "test suite for dbms"  >::: List.flatten [
    data_read_write_tests;
    queries_tests;
    select_tests;
    insert_tests;
    delete_tests;
  ]

let _ = run_test_tt_main suite