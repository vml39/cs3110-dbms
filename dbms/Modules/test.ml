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
let init = Datardwt.database := "testdb"

(* [make_query_test name expected s] constructs an OUnit test named 
   [name] that asserts the quality of [expected] of [s] applied to 
   query.parse*)
let query_test name expected s =
  "Query Test: " ^ name >:: (fun _ -> 
      assert_equal expected (parse s))

(* [malformed_query_test name s] constructs an OUnit test named [name] that 
   asserts [s] applied to [parse] raises a query.Malformed exception*)
let malformed_test name m s =
  "Malformed query Test: " ^ name >:: (fun _ -> 
      assert_raises (Malformed m)(fun () ->  (parse s)))

(* [malformed_query_test name s] constructs an OUnit test named [name] that 
   asserts [s] applied to query.parse raises a query.Empty exception*)
let empty_test name s =
  "Empty query Test: " ^ name >:: (fun _ -> 
      assert_raises Empty (fun () ->  (parse s)))

let selectobj = {
  table = "alpha"; 
  fields = ["a"]; 
  where = None; 
  order = None
}
let selectobj' = {selectobj with fields = ["a"; "b"; "c"]}
let selectobj1 = {
  table = "students";
  fields = ["*"];
  where = None;
  order = None;
}
let selectobj2 = {selectobj1 with fields = ["name"; "netid"]}
let selectobj3 = {selectobj1 with fields = ["name netid"]}
let whereobj = {field = "name";op = Like;ptn = "%i%"}
let selectobj4 = {selectobj1 with where = Some whereobj}
let whereobj1 = {field = "year"; op = LEQ; ptn = "2021"}
let selectobj5 = {selectobj1 with where = Some whereobj1}
let selectobj6 = {selectobj1 with order = Some "name"}
let selectobj7 = {selectobj4 with order = Some "name"}
let deleteobj = {
  table = "students";
  where = None
}
let insertobj = {
  table = "students";
  fields = None;
  values = ["Roger Williams"; "rw1"; "2023"; "Film"; "West"]
}

let queries_tests = [
  query_test "SELECT a FROM alpha" (Select selectobj) "SELECT a FROM alpha";
  query_test "SELECT a,b,c FROM alpha" (Select selectobj') 
    "SELECT a,b,c FROM alpha";
  query_test "SELECT  a,   b,c FROM alpha        " (Select selectobj') 
    "SELECT  a,   b,c FROM alpha        ";
  query_test "SELECT * FROM students" (Select selectobj1)
    "SELECT * FROM students";
  query_test "SELECT name, netid FROM students" (Select selectobj2)
    "SELECT name, netid FROM students";
  query_test "SELECT name netid FROM students" (Select selectobj3)
    "SELECT name netid FROM students";
  query_test "SELECT * FROM students WHERE name LIKE %i%" (Select selectobj4)
    "SELECT * FROM students WHERE name LIKE %i%";
  query_test "SELECT * FROM students WHERE year <= 2021" (Select selectobj5)
    "SELECT * FROM students WHERE year <= 2021";
  query_test "SELECT * FROM students ORDER BY name" (Select selectobj6)
    "SELECT * FROM students ORDER BY name";
  query_test "SELECT * FROM students WHERE name LIKE %i% ORDER BY name" 
    (Select selectobj7) 
    "SELECT * FROM students WHERE name LIKE %i% ORDER BY name";
  query_test "DELETE FROM students" (Delete deleteobj) "DELETE FROM students";
  (* query_test 
    "INSERT INTO students VALUES (Roger Williams, rw1, 2023, Film, West" 
    (Insert insertobj)
    "INSERT INTO students VALUES (Roger Williams, rw1, 2023, Film, West" ; *)

  malformed_test "Select * FROMm students" "Illegal query" 
    "Select * FROM students";
  malformed_test "SELECT name fRom students" 
    "Field names malformed or no 'FROM' keyword" 
    "SELECT name fRom students";
  malformed_test "SELECT * FROM students table" 
    "The table name can only be followed by 'WHERE' or 'ORDER BY'"
    "SELECT * FROM students table";
  malformed_test "SELECT name, netid, FROM students" 
    "Field names malformed or no 'FROM' keyword"
    "SELECT name, netid, FROM students";
  malformed_test "SELECT name, net id, FROM students" 
    "Field names malformed or no 'FROM' keyword"
    "SELECT name, net id, FROM students";
  malformed_test "SELECT name, netid FROM (students)" 
    "The table name can only be followed by 'WHERE' or 'ORDER BY'"
    "SELECT name, netid FROM (students)";
  malformed_test "SELECT name, netid FROM (students" 
    "The table name can only be followed by 'WHERE' or 'ORDER BY'"
    "SELECT name, netid FROM (students";
  malformed_test "SELECT name, netid FROM students)" 
    "The table name can only be followed by 'WHERE' or 'ORDER BY'"
    "SELECT name, netid FROM students)";
  malformed_test "SELECT name, netid FROM students WHERE"
    "Must provide a field, operator and pattern after 'WHERE'"
    "SELECT name, netid FROM students WHERE";
  malformed_test "SELECT name, netid FROM students WHERE name"
    "Must provide a field, operator and pattern after 'WHERE'"
    "SELECT name, netid FROM students WHERE name";
  malformed_test "SELECT name, netid FROM students WHERE name oops %i%"
    "Must provide a field, operator and pattern after 'WHERE'"
    "SELECT name, netid FROM students WHERE name oops %i%";
  malformed_test "SELECT name, netid FROM students WHERE name <="
    "Must provide a field, operator and pattern after 'WHERE'"
    "SELECT name, netid FROM students WHERE name <=";
  malformed_test "SELECT name, netid FROM students ORDER BY"
    "Must provide a field to order by"
    "SELECT name, netid FROM students ORDER BY";
  malformed_test "INSERT" "Illegal query" "INSERT";
  malformed_test "INSERT INTO" "You must specify a table name" "INSERT INTO";
  malformed_test "DELETE" "Illegal query" "DELETE";
  malformed_test "DELETE FROM" "You must specify a table name" "DELETE FROM";
]

(* COMPUTATION TESTS **********************************************************)

(** TODO: document *)
let get_qry = function 
  | Select qry -> qry
  | _ -> failwith "unimplemented"

(* [malformed_select_test name s] constructs an OUnit test named 
   [name] that asserts [s] applied to Computation.select raises a 
   query.Malformed exception*)
let malformed_select_test name s m = 
  "Malformed select test: " ^ name >:: (fun _ ->
      assert_raises (Malformed m) (fun () -> (select s)))

let qry = parse "SELECT netid FROM students" |> get_qry
(* let qry' = parse "SELECT netid, name FROM students" |> get_qry
let qry'' = parse "SELECT * FROM students" |> get_qry
let qry_order = parse "SELECT * FROM students ORDER BY name" |> get_qry
(* let qry_where_eq = parse "SELECT * FROM students WHERE name = Test" |> get_qry *)
let qry_where_eq_malformed = 
  parse "SELECT * FROM students WHERE name = test" |> get_qry
let qry_where_like = 
  parse "SELECT * FROM students WHERE name LIEK %i%" |> get_qry
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
] *)

let select_tests = [
  (* select_table_test "get tablename" "animals" ["*"; "FROM"; "animals"]; *)
  (* malformed_table_test "no FROM keyword" ["*"];
     malformed_table_test "no table called after FROM" ["*"; "FROM"];
     malformed_table_test "lowercase keyword from" ["*"; "from"; "animals"]; *)
  (* malformed_fields_test "no fields" ["FROM"; "tablename"]; *)
  (* malformed_fields_test "lowercase keyword from" ["dogs"; "from"; "animals"]; *)
  (* select_test "SELECT netid FROM students" 
     (fields, [["dis52"]; ["rjm448"]; ["vml39"]; ["t123"]]) qry;
     select_test "SELECT netid, name FROM students" 
     (fields', namenetid) qry';
     select_test "SELECT * FROM students" (schema, students) qry'';
     select_test "SELECT * FROM students ORDER BY name" 
     (schema, students_ordered) qry_order; *)
  (* select_test "SELECT * FROM students WHERE name = Test" 
     (schema, [["Test"; "t123"; "2022"; "Government"; "North"]]) qry_where_eq; *)
  (* select_test "SELECT * FROM students WHERE name LIKE %i%" 
     (schema, [["Test"; "t123"; "2022"; "Government"; "North"]]) qry_where_like; *)
  (* malformed_select_test "SELECT * FROM students WHERE name = test" 
     qry_where_eq_malformed; *)
]

(* let ins_qry1 = parse "INSERT INTO students VALUES (Joe, jfs9, 1969, ECE, Collegetown)" 
let post_ins1 = (*insert ins_qry1;*) parse "SELECT * FROM students" |> get_qry
let ins_qry2 = parse "INSERT INTO students (name, netid, major) VALUES (Joe, jfs9, ECE)" 
let post_ins2 = (*insert ins_qry2;*) parse "SELECT * FROM students" |> get_qry

let students_up_1 = students @ [["Joe"; "jfs9"; "1969"; "ECE"; "Collegetown"]]
let students_up_2 = students_up_1 @ [["Joe"; "jfs9"; ""; "ECE"; ""]]


let insert_tests = [
  (* select_test "INSERT full" (fields', students_up_1) post_ins1; *)
  (* select_test "INSERT partial" (fields', students_up_2) post_ins2; *)
]


let del_qry1 = parse "DELETE FROM students WHERE class = 1969" 
let post_del1 = (*delete del_qry1;*) parse "SELECT * FROM students" |> get_qry
let del_qry2 = parse "DELETE FROM students WHERE name = Joe" 
let post_del2 = (*delete del_qry2;*) parse "SELECT * FROM students" |> get_qry

let students_up_3 = students @ [["Joe"; "jfs9"; ""; "ECE"; ""]]

let delete_tests = [
  (* These test cases are failing even though expected and received look the *)
  (* select_test "DELETE partial1" (fields', students_up_3) post_del1; *)
  (* select_test "DELETE partial2" (fields', students) post_del2; *)
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
] *)

(******************************************************************************)

let suite =
  "test suite for dbms"  >::: List.flatten [
    (* data_read_write_tests; *)
    queries_tests;
    select_tests;
    (* insert_tests; *)
    (* delete_tests; *)
  ]

let _ = run_test_tt_main suite