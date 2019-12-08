open Query
open Datardwt

let rec pp_list = function 
  | [] -> ()
  | h::t -> print_string (h^", "); pp_list t

let rec pp_list_bool = function 
  | [] -> ()
  | h::t -> 
    if h then (print_string ("true , "); pp_list_bool t)
    else (print_string ("false , "); pp_list_bool t)

let rec pp_list_list = function 
  | [] -> ()
  | h::t -> print_newline (pp_list h); pp_list_list t

(** [index field schema] is the index of [field] in list [schema]. *)
let index field schema =
  let i = ref (-1) in 
  List.fold_left 
    (fun ind x -> i := !i + 1; if x = field then !i else ind) 0 schema

(** [get_field s] is (table, field) parsed from [s], separated by '.'. *)
let get_field s = 
  let len = String.length s in 
  let ind = String.index s '.' in 
  (String.sub s 0 ind, String.sub s (ind+1) (len-ind-1))

(** [check_fields schema fields] is [unit] if field in [fields] is in [schema]. 
    Raises [Malformed] if any [fields] are not in [schema]. *)
let rec check_fields schema = function 
  | [] -> () 
  | h::t -> 
    if List.mem h schema then check_fields schema t 
    else raise (Malformed "Field(s) selected not in schema")

(** [check_fields_join table1 table2 schema1 schema2 acc1 acc2 fields] is the 
    list of field names from [table1] and [table2] in the form 
    (fields1, fields2) parsed from [fields]. 
    Raises [Malformed] if any [fields] are not in [schema1] or [schema2]. *)
let rec check_fields_join table1 table2 schema1 schema2 acc1 acc2 = function
  (* order_fields schema bool_fields *)
  | [] -> List.rev acc1, List.rev acc2
  | h::t ->
    if fst (get_field h) = table1 && List.mem (snd (get_field h)) schema1
    then check_fields_join table1 table2 schema1 schema2 
        ((snd (get_field h))::acc1) acc2 t
    else if fst (get_field h) = table2 && List.mem (snd (get_field h)) schema2
    then check_fields_join table1 table2 schema1 schema2 
        acc1 (snd (get_field h)::acc2) t
    else raise (Malformed "Field selected is not part of either table in JOIN")

(** [select_fields schema fields] is [schema] if [fields] is [[*]] and [fields]
    otherwise. 
    Raises [Malformed] if not all [fields] are in [schema]. *)
let select_fields schema fields = 
  if fields = ["*"] then schema
  else (check_fields schema fields; fields)

(** TODO: document *)
let select_fields_join table1 table2 schema fields = 
  if fields = ["*"] then (fst schema, snd schema)
  else check_fields_join table1 table2 (fst schema) (snd schema) [] [] fields

(** TODO: document *)
(* let rec print_fields schema bfields acc = 
   match schema, bfields with 
   | [], [] -> List.rev acc
   | h::t, h'::t' -> 
    if h' then print_fields t t' (h::acc) 
    else print_fields t t' acc
   | _ -> failwith "inequal num of fields" *)

let rec table_schema db_schema tablename = 
  match db_schema with 
  | [] -> 
    raise (Malformed "Table given does not exist")
  | h::t -> if fst h = tablename then snd h else table_schema t tablename

(** [filter_fields fields acc schema] is a bool list [acc] where each elt 
    corresponds to a field in [schema], where the elt is [true] if the field 
    is in [fields] and false otherwise. *)
let rec filter_fields schema fields acc = 
  List.map (fun x -> if List.mem x fields then true else false) schema

(** [order_fields schema fields] is [fields] ordered according to [schema]. *)
let rec order_fields schema fields = 
  let i = ref (-1) in
  List.filter(fun x -> i := !i + 1; List.nth fields !i) schema

(** [order_fields_join schema fields] is... *)
let order_fields_join schema fields = 
  let bfields1, bfields2 = 
    (filter_fields (fst schema) (fst fields) [], 
     filter_fields (snd schema) (snd fields) [])
  in (order_fields (fst schema) bfields1)@(order_fields (snd schema) bfields2)

(** [convert_to_regex pattern] is the SQL [pattern] converted to an OCaml 
    regex pattern. *)
let rec convert_to_regex = function (* TODO: Remove this function and consolidate below *)
  | [] -> ""
  | h::t when h = "%" -> ".*" ^ convert_to_regex t
  | h::t when h = "_" -> "." ^ convert_to_regex t
  | h::t -> h ^ convert_to_regex t

(** [parse_pattern pattern] is the SQL [pattern] following the "LIKE" operator 
    in the query and coverted to an OCaml regex pattern. *)
let parse_pattern pattern = 
  let patternList = 
    pattern
    |> Str.global_replace (Str.regexp "_") " _ " 
    |> Str.global_replace (Str.regexp "%") " % " 
    |> String.split_on_char ' ' 
    |> List.filter ( fun s -> s <> "") in 
  convert_to_regex patternList

(* [filter_pattern fields row ind pattern i operator] is [Some row] with only 
   the fields specified in [fields].  Returns the row if the row follows the SQL
   [pattern]. Returns [None] if the row does not follow the SQL [pattern] *)
let filter_pattern fields row ind pattern i operator = 
  if operator (List.nth row ind) pattern
  then Some (List.filter (fun _ -> i := !i + 1; List.nth fields !i) row)
  else None

(** [filter_row schema fields where pattern row] is [Some row] with only the 
    fields specified in [fields]. Returns the row if [where] is [false] or if 
    [where] is [true] and the row follows the SQL [pattern]. Returns [None] if
    [where] is [true] and the row does not follow the SQL [pattern]. *)
let filter_row schema fields (where: Query.where_obj option) row =
  let i = ref (-1) in 
  match where with 
  | None -> Some (List.filter (fun _ -> i := !i + 1; List.nth fields !i) row)
  | Some where -> 
    let ind = index where.field schema in
    let partial_filter_pattern = filter_pattern fields row ind where.ptn i in
    match where.op with
    | Like ->
      if Str.string_match (Str.regexp (parse_pattern where.ptn)) (List.nth row ind) 0
      then Some (List.filter (fun _ -> i := !i + 1; List.nth fields !i) row)
      else None
    | EQ -> partial_filter_pattern (=)
    | NEQ -> partial_filter_pattern (<>)
    | GT -> partial_filter_pattern (>)
    | LT -> partial_filter_pattern (<)
    | GEQ -> partial_filter_pattern (>=)
    | LEQ -> partial_filter_pattern (<=)
    | s -> failwith "Expected a valid operator"

(* else Some (List.filter (fun _ -> i := !i + 1; List.nth fields !i) row) *)

(** [filter_table fc schema fields where acc] is the table constructed
    from filtering the [fields] from each row of [fc]. *)
let rec filter_table fc (schema: string list) fields (where: Query.where_obj option) acc = 
  let row = try read_next_line fc |> filter_row schema fields where with 
    | exn -> Stdlib.close_in fc; Some []
  in match row with 
  | None -> filter_table fc schema fields where acc
  | Some e when e = [] -> List.rev acc 
  | Some r -> filter_table fc schema fields where (r::acc)

(** [comp n x y] is [-1] if the [n]th element of [x] is less than the [n]th value 
    of [y] using the Stdlib compare function; [0] if they are equal; and [1] if 
    the [n]th value of [x] is greater than the [n]th value of [y]. *)
let comp n x y = 
  let x' = List.nth x n in 
  let y' = List.nth y n in 
  Stdlib.compare x' y'

(** [order table schema qry table] is [table] with rows sorted by the the field
    following the "ORDER BY" keyword in [qry]. *)
let order schema (qry_order: Query.fieldname option) table = 
  match qry_order with 
  | None -> table
  | Some field -> List.sort (comp (index field schema)) table

(** [like_equal fc schema fields qry] is the OCaml table constructed from the
    rows in [fc] based on the "WHERE" condition in [qry]. Table only contains
    the fields specified in [fields] from the table [schema]. *)
let rec like_equal fc schema fields (qry_where : Query.where_obj) = 
  filter_table fc schema fields (Some qry_where) []

(** [where tablename qry schema fields] is the OCaml table created from parsing 
    each row in the database table with [tablename]. Table results are filtered
    if there is a "WHERE" keyword in [qry]. Table only contains the fields
    specified in [fields] from the table [schema]. *)
let where tablename (qry_where : Query.where_obj option) schema fields fc = 
  match qry_where with
  | None -> filter_table fc schema fields None [] 
  | Some w -> like_equal fc schema fields w

(** [filter_row2 schema fields row] is... *)
let filter_row2 schema fields row =
  let bool_fields = filter_fields schema fields [] in
  match filter_row schema bool_fields None row with 
  | None -> []
  | Some r -> r

(** [get_cond field schema row] is... *)
let get_cond field schema row = 
  index (snd (get_field field)) schema |> List.nth row

(** [filter_table2 qry_join cond field_index fc1] is...  *)
let rec filter_table2 qry_join cond field_index schema2 fields fc1 = 
  try 
    let row = read_next_line fc1 in 
    if List.nth row field_index = cond 
    then Some (filter_row2 schema2 (snd fields) row)
    else filter_table2 qry_join cond field_index schema2 fields fc1
  with 
  | exn -> Stdlib.close_in fc1; None

(** [filter_row_join qry qry_join schema schema2 fields row] is...  *)
let filter_row_join qry (qry_join : join_obj) schema fields row : string list option = 
  (* match qry.where with 
     (* get only the rows from each table that we need *)
     | None -> (List.filter (fun _ -> i := !i + 1; List.nth fields !i) row)
     (* run rows through where condition *)
     | Some ->  *)
  let fc1 = get_in_chan qry_join.table in 
  let cond = get_cond (fst qry_join.on) (fst schema) row in 
  let field_ind = index (snd (get_field (snd qry_join.on))) (snd schema) in 
  match filter_table2 qry.join cond field_ind (snd schema) fields fc1 with 
  | None -> None
  | Some r' -> let r = filter_row2 (fst schema) (fst fields) row in 
    Some (r@r')

(** [inner_join qry qry_join schema1 schema2 fields fc acc] is... *)
let rec inner_join qry qry_join schema fields fc acc = 
  let row = try read_next_line fc |> filter_row_join qry qry_join schema fields with 
    | exn -> Stdlib.close_in fc; Some []
  in match row with 
  | None -> inner_join qry qry_join schema fields fc acc
  | Some e when e = [] -> List.rev acc 
  | Some r -> inner_join qry qry_join schema fields fc (r::acc)

(** [join qry qry_join schema1 schema2 fields fc] is... *)
let join (qry: Query.select_obj) (qry_join: Query.join_obj) schema fields fc = 
  match qry_join.join with 
  | Inner ->  inner_join qry qry_join schema fields fc []
  | Left -> failwith "left" (* return all rows from left table and have null columns if something doesn't exist in right table *)
  | Right -> failwith "right" (* opposite of left *)
  | Outer -> failwith "outer" (* returns results from both *)
  | None -> raise (Malformed "Must provide a type of join")

let select (qry : Query.select_obj) =
  let tablename = qry.table in 
  let schema = table_schema (schema_from_txt ()) tablename in 
  let fc = get_in_chan tablename in 
  match qry.join with 
  | None -> 
    let fields = select_fields schema qry.fields in 
    let bool_fields = filter_fields schema fields [] in
    let table = where tablename qry.where schema bool_fields fc in 
    (order_fields schema bool_fields, order schema qry.order table)
  | Some join_obj -> 
    let schema1 = table_schema (schema_from_txt ()) join_obj.table in 
    let fields' = 
      select_fields_join qry.table join_obj.table (schema, schema1) qry.fields 
    in 
    (order_fields_join (schema, schema1) fields', 
     join qry join_obj (schema, schema1) fields' fc)

(* INSERT *)

(** TODO: document *)
let rec vals_update sch cols vals acc =
  match sch, cols with
  | [], [] -> List.rev acc
  | [], h :: t -> raise (Malformed "scoop")
  | h :: t, [] -> vals_update t cols vals ("" :: acc)
  | h1 :: t1, h2 :: t2 -> if h1 = h2 
    then vals_update t1 t2 (List.tl vals) (List.hd vals :: acc)
    else vals_update t1 (h2 :: t2) vals ("" :: acc)

let insert (qry: insert_obj) = 
  let schema = table_schema (schema_from_txt ()) qry.table in 
  match qry.fields with 
  | None -> if List.length qry.values <> List.length schema 
    then raise (Malformed "Values given do not match Schema")
    else begin
      (* you have all cols so insert them all*)
      let outc = get_out_chan qry.table in
      write_line outc qry.values; 
      close_out outc
    end
  | Some lst -> 
    (* step through schema and vals, inserting empty strings where necessary,
       then write that*)
    let writable = vals_update schema lst qry.values [] in
    let outc = get_out_chan qry.table in
    write_line outc writable; 
    close_out outc

(* DELETE *)

let rec delete_helper inc outc schema ind op ptn =
  try let line = read_next_line inc in
    if op (List.nth line ind) ptn
    then delete_helper inc outc schema ind op ptn
    else write_line outc line; 
    delete_helper inc outc schema ind op ptn
  with | End_of_file -> ()

let rec delete_helper_like inc outc schema ind ptn =
  try let line = read_next_line inc in
    if Str.string_match (Str.regexp (parse_pattern ptn)) (List.nth line ind) 0
    then delete_helper_like inc outc schema ind ptn
    else write_line outc line; 
    delete_helper_like inc outc schema ind ptn
  with | End_of_file -> ()

let delete qry = 
  match qry.where with
  | None -> begin 
      let outc = open_out (get_path qry.table) in
      output_string outc "";
      close_out outc
    end
  | Some where_rec -> begin
      let temp_file = qry.table ^ ".tmp" in
      let outc = get_out_chan temp_file in
      let inc = get_in_chan qry.table in
      let schema = table_schema (schema_from_txt ()) qry.table in
      let ind = index where_rec.field schema in 
      (* for each row*)
      if where_rec.op = Like 
      then begin
        delete_helper_like inc outc schema ind where_rec.ptn;
        close_in inc;
        close_out outc;
        Sys.remove (get_path qry.table);
        Sys.rename (get_path temp_file) (get_path qry.table)
      end
      else 
        let oper =
          match where_rec.op with
          | EQ -> (=)
          | GT -> (>)
          | LT -> (<)
          | GEQ -> (>=)
          | LEQ -> (=)
          | Like -> (fun s1 s2 -> true)
          | NEQ -> (<>)
          | None -> failwith "Expected a valid operator"
        in
        delete_helper inc outc schema ind oper where_rec.ptn;
        close_in inc;
        close_out outc;
        Sys.remove (get_path qry.table);
        Sys.rename (get_path temp_file) (get_path qry.table)
    end

let rec create_table (qry: Query.create_obj) = 
  let outc_schema = get_out_chan_schema () in
  write_line_table_schema outc_schema qry.table qry.fields; 
  close_out outc_schema;
  let outc_tables = get_out_chan qry.table in 
  close_out outc_tables

let rec drop_helper inc outc name = 
  try let found_name, line = read_next_schema_line inc in
    if name = found_name
    then drop_helper inc outc name
    else write_line_table_schema outc (found_name) (line); 
    drop_helper inc outc name
  with | End_of_file -> ()

let drop_table qry = 
  let schema = table_schema (schema_from_txt ()) qry.table in 
  let outc_schema = get_out_chan_temp_schema () in
  let inc_schema = get_in_chan_schema () in
  output_string outc_schema (List.hd (read_next_line inc_schema) ^ "\n");
  drop_helper inc_schema outc_schema qry.table;
  close_out outc_schema;
  close_in inc_schema;
  Sys.remove (get_schema_path ());
  Sys.rename (get_schema_temp_path ()) (get_schema_path ());
  Sys.remove (get_path qry.table)

let general_msg = 
  "'HELP' Command \n"
  ^ "To get help for a command, type HELP and the command name \n"
  ^ "The list of valid commands is: SELECT, INSERT INTO, DELETE, TRUNCATE"
  ^ " TABLE, CREATE TABLE, DROP TABLE, CHANGE DATABASE, READ, HELP, and QUIT\n"

let select_msg = 
  "'SELECT' QUERY \n"

let insert_msg = 
  "'INSERT INTO' QUERY \n"
  ^ "INSERT INTO inserts a new row into a table inserting data into the"
  ^ " specified coulmns\n"
  ^ "USAGE: 1) INSERT INTO [tablename] (f1, f2,... fn) VALUES (v1, v2,... vn)\n"
  ^ "       2) INSERT INTO [tablename] VALUES (v1, v2,... vn)*\n"
  ^ "Requires: [tablename] is the name of an existing table and" 
  ^ "(f1, f2,... fn) are colmum names in [tablename]\n"
  ^ "*For this case, length of (v1, v2,... vn) must match the number of"
  ^ " columns\n"

let delete_msg = 
  "'DELETE' QUERY \n"
  ^ "DELETE deletes rows from a table, optionally meeting a given condition\n"
  ^ "USAGE: 1) DELETE FROM [tablename]\n"
  ^ "       2) DELETE FROM [tablename] WHERE [b]\n"
  ^ "Requires: [tablename] is the name of an existing table and"
  ^ "b is a conditional following WHERE guidelines\n"

let truncate_msg = 
  "'TRUNCATE TABLE' QUERY \n"
  ^ "TRUNCATE TABLE removes all data from a specified table\n"
  ^ "USAGE: TRUNCATE TABLE [tablename]\n"
  ^ "Requires: [tablename] is the name of an existing table\n"

let create_msg = 
  "'CREATE TABLE' QUERY \n"
  ^ "CREATE TABLE creates a new, empty table with the specified name\n"
  ^ "USAGE: CREATE TABLE [tablename]\n"
  ^ "Requires: [tablename] is a valid table name (no whitespace, commas,)\n"

let drop_msg = 
  "'DROP TABLE' QUERY \n"
  ^ "DROP TABLE removes a specified table from the database\n"
  ^ "USAGE: DROP TABLE [tablename]\n"
  ^ "Requires: [tablename] is the name of an existing table\n"

let change_msg = 
  "'CHANGE DATABASE' Command \n"
  ^ "CHANGE DATABASE changes the working database\n"
  ^ "USAGE: CHANGE DATABASE [databasename]\n"
  ^ "Requires: [databasename] is the name of an existing database visible to"
  ^ " the system\n"

let read_msg = 
  "'READ' Command \n"
  ^ "READ reads queries from an input file and sends their results to an output"
  ^ "file\n"
  ^ "USAGE: READ FROM [filename]\n"
  ^ "Requires: [filename] is the name of an existing file in the 'input' folder"
  ^ "containing properly formatted queries\n"

let quit_msg = 
  "'QUIT' Command \n"
  ^ "QUIT exits the dbms\n"
  ^ "USAGE: QUIT\n"

let help_msg = 
  "'HELP' Command \n"

let where_msg = 
  "'WHERE' Keyword \n"

let join_msg = 
  "'JOIN' Keyword \n"

let order_msg = 
  "'ORDER BY' Keyword \n"

let help_with qry =
  match qry.s1, qry.s2 with
  | "", "" -> general_msg
  | "SELECT", _ -> select_msg
  | "INSERT", _ -> insert_msg
  | "DELETE", _ -> delete_msg
  | "TRUNCATE", _ -> truncate_msg
  | "CREATE", _ -> create_msg
  | "DROP", _ -> drop_msg
  | "CHANGE", _ -> change_msg
  | "READ", _ -> read_msg
  | "QUIT", _ -> quit_msg
  | "HELP", _ -> general_msg
  | "WHERE", _ -> where_msg
  | "JOIN", _ -> join_msg
  | "ORDER", "BY" -> order_msg
  | _ -> "Sorry, I can't help you with that\n"