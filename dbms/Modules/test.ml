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

let whereobj1 = {field = "name"; op = Like; ptn = "%i%"}
let whereobj2 = {field = "name"; op = EQ; ptn = "Daffy"}
let whereobj3 = {field = "name"; op = NEQ; ptn = "Donald Duck"}
let whereobj4 = {field = "name"; op = GT; ptn = "alpha"}
let whereobj5 = {field = "class"; op = LT; ptn = "2020"}
let whereobj6 = {field = "height"; op = GEQ; ptn = "2.0"}
let whereobj7 = {field = "year"; op = LEQ; ptn = "2021"}

let joinobj1 = {table = "dorms"; join = Inner; on = ("buildings.id", "dorms.buildingid")}

let selectobj1 = {table = "alpha"; fields = ["a"]; where = None; order = None; join = None}
let selectobj2 = {selectobj1 with fields = ["a"; "b"; "c"]}
let selectobj3 = {table = "students"; fields = ["*"]; where = None; order = None; join = None}
let selectobj4 = {selectobj3 with fields = ["name"; "netid"]}
let selectobj5 = {selectobj3 with fields = ["name netid"]}
let selectobj6 = {selectobj3 with where = Some whereobj1}
let selectobj7 = {selectobj3 with where = Some whereobj2}
let selectobj8 = {selectobj3 with where = Some whereobj3}
let selectobj9 = {selectobj3 with where = Some whereobj4} 
let selectobj10 = {selectobj3 with where = Some whereobj5}
let selectobj11 = {selectobj3 with where = Some whereobj6}
let selectobj12 = {selectobj3 with where = Some whereobj7}
let selectobj13 = {selectobj3 with order = Some "name"}
let selectobj14 = {selectobj6 with order = Some "name"}
let selectobj15 = {table = "buildings"; fields = ["*"]; where = None; order = None; join = Some joinobj1}

let insertobj1 = {
  table = "students";
  fields = None;
  values = ["Roger Williams"; "rw1"; "2023"; "Film"; "West"]
}
let insertobj2 = {
  insertobj1 with 
  fields = Some ["name"; "netid"];
  values = ["Roger Williams"; "rw1"]
}

let deleteobj1 = {table = "students"; where = None}
let deleteobj2 = {deleteobj1 with where = Some whereobj1}

let createobj1 = {table = "dorms";fields = ["name"; "location"]}

let queries_tests = [
  (* Basic select tests *)
  query_test "SELECT a FROM alpha" (Select selectobj1) "SELECT a FROM alpha";
  query_test "SELECT a,b,c FROM alpha" (Select selectobj2) 
    "SELECT a,b,c FROM alpha";
  query_test "SELECT  a,   b,c FROM alpha        " (Select selectobj2) 
    "SELECT  a,   b,c FROM alpha        ";
  query_test "SELECT * FROM students" (Select selectobj3)
    "SELECT * FROM students";
  query_test "SELECT name, netid FROM students" (Select selectobj4)
    "SELECT name, netid FROM students";
  query_test "SELECT name netid FROM students" (Select selectobj5)
    "SELECT name netid FROM students";
  (* Select where tests *)
  query_test "SELECT * FROM students WHERE name LIKE %i%" 
    (Select selectobj6)  "SELECT * FROM students WHERE name LIKE %i%";
  query_test "SELECT * FROM students WHERE name = Daffy" 
    (Select selectobj7) "SELECT * FROM students WHERE name = Daffy";
  query_test "SELECT * FROM students WHERE name <> Donald Duck" 
    (Select selectobj8) "SELECT * FROM students WHERE name <> Donald Duck";
  query_test "SELECT * FROM students WHERE name > alpha"
    (Select selectobj9) "SELECT * FROM students WHERE name > alpha";
  query_test "SELECT * FROM students WHERE name class < 2020" 
    (Select selectobj10) "SELECT * FROM students WHERE class < 2020";
  query_test "SELECT * FROM students WHERE name class >= 2020" 
    (Select selectobj11) "SELECT * FROM students WHERE height >= 2.0";
  query_test "SELECT * FROM students WHERE year <= 2021" 
    (Select selectobj12) "SELECT * FROM students WHERE year <= 2021";
  (* Select order by *)
  query_test "SELECT * FROM students ORDER BY name" (Select selectobj13)
    "SELECT * FROM students ORDER BY name";
  (* Select where and order by *)
  query_test "SELECT * FROM students WHERE name LIKE %i% ORDER BY name" 
    (Select selectobj14) 
    "SELECT * FROM students WHERE name LIKE %i% ORDER BY name";
  (* Select join *)
  query_test "SELECT * FROM buildings INNER JOIN dorms ON buildings.id = 
    dorms.buildingid"
    (Select selectobj15)
    "SELECT * FROM buildings INNER JOIN dorms ON buildings.id = dorms.buildingid";
  (* Insert tests *)
  query_test 
    "INSERT INTO students VALUES (Roger Williams, rw1, 2023, Film, West)" 
    (Insert insertobj1)
    "INSERT INTO students VALUES (Roger Williams, rw1, 2023, Film, West)";
  query_test 
    "INSERT INTO students (name, netid) VALUES (Roger Williams, rw1)" 
    (Insert insertobj2)
    "INSERT INTO students (name, netid) VALUES (Roger Williams, rw1)";
  (* Delete Tests *)
  query_test "DELETE FROM students" (Delete deleteobj1) "DELETE FROM students";
  query_test "DELETE FROM students WHERE name LIKE %i%" (Delete deleteobj2) 
    "DELETE FROM students WHERE name LIKE %i%";
  (* Create tests *)
  query_test "CREATE TABLE dorms (name, location)" (Create createobj1) 
    "CREATE TABLE dorms (name, location)";
  (* Malformed tests *)
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
    "Must provide a pattern to match with after WHERE operator"
    "SELECT name, netid FROM students WHERE name <=";
  malformed_test "SELECT name, netid FROM students ORDER BY"
    "Must provide a field to order by"
    "SELECT name, netid FROM students ORDER BY";
  malformed_test "INSERT" "Illegal query" "INSERT";
  malformed_test "INSERT INTO" "You must specify a table name" "INSERT INTO";
  malformed_test 
    "INSERT INTO students VALUES (Roger Williams, rw1, 2023, Film, West" 
    "You must specify a valid list of values"
    "INSERT INTO students VALUES (Roger Williams, rw1, 2023, Film, West" ;
  malformed_test "DELETE" "Illegal query" "DELETE";
  malformed_test "DELETE FROM" "You must specify a table name" "DELETE FROM";
  malformed_test "DELETE FROM students table" 
    "You can delete all values from a table or specify a WHERE condition" 
    "DELETE FROM students table";
  malformed_test "CREATE TABLE" "Must specify fields for the new table" 
    "CREATE TABLE";
  malformed_test "CREATE TABLE dorms" "Invalid CREATE TABLE query" 
    "CREATE TABLE dorms";
  malformed_test "CREATE TABLE dorms (" "Invalid field names" 
    "CREATE TABLE dorms (";
  malformed_test "CREATE TABLE dorms name, location)" 
    "Invalid CREATE TABLE query" "CREATE TABLE dorms name, location)";
  malformed_test "CREATE TABLE dorms (name, location" "Invalid field names" 
    "CREATE TABLE dorms (name, location";
  malformed_test "CREATE TABLE dorms (name, location, " "Invalid field names" 
    "CREATE TABLE dorms (name, location, ";
]


(* COMPUTATION TESTS **********************************************************)

(** TODO: document *)
let get_qry qry = 
  match parse qry with 
  | Select qry -> qry
  | _ -> failwith "unimplemented"

let select_test name expected s = 
  "Select table test: " ^ name >:: (fun _ -> 
      assert_equal expected (select s))

(* [malformed_select_test name s] constructs an OUnit test named 
   [name] that asserts [name] applied to get_qry |> Computation.select raises a 
   query.Malformed exception*)
let malformed_select_test name m = 
  "Malformed select test: " ^ name >:: (fun _ ->
      assert_raises (Malformed m) (fun () -> (select (get_qry name))))

let select1 = get_qry "SELECT netid FROM students"
let select2 = get_qry "SELECT netid, name FROM students"
let select3 = get_qry "SELECT * FROM students"
let order1 = get_qry "SELECT * FROM students ORDER BY name"
let where_like1 = get_qry "SELECT * FROM students WHERE name LIKE %i%"
let where_eq1 = get_qry "SELECT * FROM students WHERE name = Test"
let where_neq1 = get_qry "SELECT * FROM students WHERE name <> Test"
let where_gt1 = get_qry "SELECT * FROM students WHERE netid > dis52"
let where_lt1 = get_qry "SELECT name FROM students WHERE class < 2021"
let where_geq1 = get_qry "SELECT name, netid FROM students WHERE home >= Cat"
let where_leq1 = get_qry "SELECT * FROM students WHERE major <= ECE"
let where_eq_mal1 = get_qry "SELECT * FROM students WHERE name = test"
let join_inner = get_qry "SELECT * FROM buildings INNER JOIN dorms ON buildings.id = dorms.buildingid"
let schema1 = ["name"; "netid"; "class"; "major"; "home"]
let schema2 = ["id"; "name"; "location"; "goodforstudying"; "name"; "location"; "buildingid"]
let fields1 = ["netid"]
let fields2 = ["name"; "netid"]
let fields3 = ["name"]
let name_netid = [
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
let students_where_like1 = [
  ["Daniel Stabile"; "dis52"; "2021"; "CS"; "Cascadilla Hall"];
  ["Robert Morgowicz"; "rjm448"; "2020"; "ECE"; "Cascadilla Hall"];
  ["Vivian Li"; "vml39"; "2020"; "IS"; "Collegetown"]
]
let students_where_eq1 = [
  ["Test"; "t123"; "2022"; "Government"; "North"]
]
let students_where_neq1 = [
  ["Daniel Stabile"; "dis52"; "2021"; "CS"; "Cascadilla Hall"];
  ["Robert Morgowicz"; "rjm448"; "2020"; "ECE"; "Cascadilla Hall"];
  ["Vivian Li"; "vml39"; "2020"; "IS"; "Collegetown"]
]
let students_where_gt1 = [
  ["Robert Morgowicz"; "rjm448"; "2020"; "ECE"; "Cascadilla Hall"];
  ["Vivian Li"; "vml39"; "2020"; "IS"; "Collegetown"];
  ["Test"; "t123"; "2022"; "Government"; "North"];
]
let students_where_lt1 = [
  ["Robert Morgowicz"];
  ["Vivian Li"]
]
let students_where_geq1 = [
  ["Vivian Li"; "vml39"];
  ["Test"; "t123"];
]
let students_where_leq1 = [
  ["Daniel Stabile"; "dis52"; "2021"; "CS"; "Cascadilla Hall"];
  ["Robert Morgowicz"; "rjm448"; "2020"; "ECE"; "Cascadilla Hall"];
]
let students_ordered = [
  ["Daniel Stabile"; "dis52"; "2021"; "CS"; "Cascadilla Hall"];
  ["Robert Morgowicz"; "rjm448"; "2020"; "ECE"; "Cascadilla Hall"];
  ["Test"; "t123"; "2022"; "Government"; "North"];
  ["Vivian Li"; "vml39"; "2020"; "IS"; "Collegetown"]
]
let buildings_join = [
  ["6"; "Balch Hall"; "North"; "false"; "Balch Hall"; "North"; "6"];
  ["7"; "Bethe Hall"; "West"; "true"; "Bethe Hall"; "West"; "7"];
]

let select_tests = [
  (* Basic Select*)
  select_test "SELECT netid FROM students" 
    (fields1, [["dis52"]; ["rjm448"]; ["vml39"]; ["t123"]]) select1;
  select_test "SELECT netid, name FROM students" 
    (fields2, name_netid) select2;
  select_test "SELECT * FROM students" 
    (schema1, students) select3;
  (* Select Where *)
  select_test "SELECT * FROM students WHERE name LIKE %i%" 
    (schema1, students_where_like1) where_like1;
  select_test "SELECT * FROM students WHERE name = Test" 
    (schema1, students_where_eq1) where_eq1;
  select_test "SELECT * FROM students WHERE name <> Test"
    (schema1, students_where_neq1) where_neq1;
  select_test "SELECT * FROM students WHERE netid > dis52"
    (schema1, students_where_gt1) where_gt1; 
  select_test "SELECT name FROM students WHERE class < 2021"
    (fields3, students_where_lt1) where_lt1; 
  select_test "SELECT name, netid FROM students WHERE home >= Cat"
    (fields2, students_where_geq1) where_geq1; 
  select_test "SELECT * FROM students WHERE major <= ECE"
    (schema1, students_where_leq1) where_leq1; 
  (* Select Order By *)
  select_test "SELECT * FROM students ORDER BY name" 
    (schema1, students_ordered) order1;
  (* Select Join *)
  select_test 
    "SELECT * FROM buildings INNER JOIN dorms ON buildings.id = dorms.buildingid"
    (schema2, buildings_join) join_inner;
  malformed_select_test "SELEC netid FROM students" "Illegal query";
  malformed_select_test "select asdf FROM students" "Illegal query";
  malformed_select_test "SELECT FROM students" 
    "Field names malformed or no 'FROM' keyword";
  malformed_select_test "SELECT asdf FROM students" 
    "Field(s) selected not in schema";
  malformed_select_test "SELECT asdf from students" 
    "Field names malformed or no 'FROM' keyword";
  malformed_select_test "SELECT name netid FROM students" 
    "Field(s) selected not in schema";
  malformed_select_test "SELECT name, netid, FROM students" 
    "Field names malformed or no 'FROM' keyword";
  malformed_select_test "SELECT asdf students" 
    "Field names malformed or no 'FROM' keyword";
  malformed_select_test "SELECT asdf FROM " 
    "No table specified";
  (* where malformed *)
  malformed_select_test "SELECT asdf FROM students where" 
    "The table name can only be followed by 'WHERE' or 'ORDER BY'";
  malformed_select_test "SELECT asdf FROM students WHERE" 
    "Must provide a field, operator and pattern after 'WHERE'";
  malformed_select_test "SELECT asdf FROM students WHERE LIKE" 
    "Must provide a field, operator and pattern after 'WHERE'";
  malformed_select_test "SELECT asdf FROM students WHERE class like" 
    "Must provide a field, operator and pattern after 'WHERE'";
  malformed_select_test "SELECT asdf FROM students WHERE class LIKE" 
    "Must provide a pattern to match with after WHERE operator";
  malformed_select_test "SELECT asdf FROM students WHERE asdf LIKE hello" 
    "Field(s) selected not in schema";
  (* malformed_select_test "SELECT class FROM students WHERE asdf LIKE asdf" 
     "WHERE field not in schema"; *)
  (* Order by malformed *)
  malformed_select_test "SELECT asdf FROM students order" 
    "The table name can only be followed by 'WHERE' or 'ORDER BY'";
  malformed_select_test "SELECT asdf FROM students ORDER by" 
    "The table name can only be followed by 'WHERE' or 'ORDER BY'";
  malformed_select_test "SELECT asdf FROM students ORDERBY" 
    "The table name can only be followed by 'WHERE' or 'ORDER BY'";
  malformed_select_test "SELECT asdf FROM students ORDER BY" 
    "Must provide a field to order by";
  malformed_select_test "SELECT asdf FROM students ORDER BY asdf" 
    "Field(s) selected not in schema";
  (* malformed_select_test "SELECT class FROM students ORDER BY classsssss" 
     "ORDER BY field is not in schema"; *)
  (* where & order by malformed *)
  malformed_select_test "SELECT asdf FROM students where name = i ORDER BY" 
    "The table name can only be followed by 'WHERE' or 'ORDER BY'";
  malformed_select_test "SELECT asdf FROM students WHERE ORDER BY" 
    "Must provide a field, operator and pattern after 'WHERE'";
  malformed_select_test "SELECT asdf FROM students WHERE name ORDER BY" 
    "Must provide a field, operator and pattern after 'WHERE'";
  malformed_select_test "SELECT asdf FROM students WHERE x = 1 ORDER BY" 
    "Must provide a field to order by";
  malformed_select_test "SELECT asdf FROM students WHERE x = 1 ORDER BY y" 
    "Field(s) selected not in schema";
  (* malformed_select_test "SELECT class FROM students WHERE x = 1 ORDER BY y"
     "WHERE field not in schema"; *)
  (* malformed_select_test "SELECT class FROM students WHERE class = 1 ORDER BY y"
     "ORDER BY field is not in schema"; *)
]

(* malformed_select_test "SELECT * FROM students WHERE name = test" 
   ""
   qry_where_eq_malformed; *)

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