open Query
open Datardwt

(* PRINT DEBUG HELPERS *)
(* [ps s] prints [s] to the Command Line. *)
let ps s = 
  print_newline (); print_string s

(* [pp_list_bool lst] prints a [lst] to the Command Line. *)
let rec pp_list = function 
  | [] -> ()
  | h::t -> print_string (h^", "); pp_list t

(* [pp_list_bool lst] prints a bool [lst] to the Command Line. *)
let rec pp_list_bool = function 
  | [] -> ()
  | h::t -> 
    if h then (print_string ("true , "); pp_list_bool t)
    else (print_string ("false , "); pp_list_bool t)

(* [pp_list_list lst] prints a string list list [lst] to the Command Line. *)
let rec pp_list_list = function 
  | [] -> ()
  | h::t -> print_newline (pp_list h); pp_list_list t

(** [select_msg] is the Malformed message if a field selected is not in the 
    schema. *)
let select_msg = "Field(s) selected not in schema"

(** [where_msg] is the Malformed message if the where field is not in the 
    schema. *)
let where_msg = "WHERE field not in schema"

(** [orderby_msg] is the Malformed message if the order by field is not in the 
    schema. *)
let orderby_msg = "ORDER BY field is not in schema"

(** [where_msg] is the Malformed message if the join field is not in either 
    schema. *)
let join_msg = "JOIN field is not in schema"

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
let rec check_fields schema msg = function 
  | [] -> () 
  | h::t -> 
    if List.mem h schema then check_fields schema msg t 
    else raise (Malformed msg)

(** [check_fields_join table1 table2 schema1 schema2 acc1 acc2 fields] is the 
    list of field names from [table1] and [table2] in the form 
    (fields1, fields2) parsed from [fields]. 
    Raises [Malformed] if any [fields] are not in [schema1] or [schema2]. *)
let rec check_fields_join table1 table2 schema1 schema2 acc1 acc2 = function
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
  else (check_fields schema select_msg fields; fields)

(** [select_fields_join table1 table2 schema fields] is the tuple of table1's 
    [schema] and table2's schema if [fields] is [[*]] and the tuple of the 
    [fields] from table1 and the [fields] from table2 otherwise. 
    Raises [Malformed] if not all [fields] are in either [schema]. *)
let select_fields_join table1 table2 schema fields = 
  if fields = ["*"] then (fst schema, snd schema)
  else check_fields_join table1 table2 (fst schema) (snd schema) [] [] fields

(** [table_schema db_schema table] is the schema for [table] from 
    [db_schema]. *)
let rec table_schema db_schema table = 
  match db_schema with 
  | [] -> 
    raise (Malformed "Table given does not exist")
  | h::t -> if fst h = table then snd h else table_schema t table

(** [filter_fields fields acc schema] is a bool list [acc] where each elt 
    corresponds to a field in [schema], where the elt is [true] if the field 
    is in [fields] and false otherwise. *)
let rec filter_fields schema fields acc = 
  List.map (fun x -> if List.mem x fields then true else false) schema

(** [order_fields schema fields] is [fields] ordered according to [schema]. *)
let rec order_fields schema fields = 
  let i = ref (-1) in
  List.filter(fun x -> i := !i + 1; List.nth fields !i) schema

(** [order_fields_join schema fields] is [fields] from both tables in a JOIN
    ordered according to their [schemas]. *)
let order_fields_join schema fields = 
  let bfields1, bfields2 = 
    (filter_fields (fst schema) (fst fields) [], 
     filter_fields (snd schema) (snd fields) [])
  in (order_fields (fst schema) bfields1)@(order_fields (snd schema) bfields2)

(* [add_boundary_characters pattern] is [pattern] with "\\b" cons onto the front
*)
let add_boundary_characters pattern = 
  match pattern with
  | h::t  -> "\\b":: pattern
  | [] -> pattern

(** [convert_to_regex pattern] is the SQL [pattern] converted to an OCaml 
    regex pattern. *)
let rec convert_to_regex pattern = 
  match pattern with
  | [] -> ""
  | h::t when h = "%" -> ".*" ^ convert_to_regex t
  | h::t when h = "_" -> "." ^ convert_to_regex t
  | h::t -> h ^ convert_to_regex t

(** [parse_pattern pattern] is the SQL [pattern] following the "LIKE" operator 
    in the query and coverted to an OCaml regex pattern. 
    Requires: pattern to be parsed cannot include '~'*)
let parse_pattern pattern = 
  let patternList = 
    pattern
    |> Str.global_replace (Str.regexp "_") "~_~" 
    |> Str.global_replace (Str.regexp "%") "~%~" 
    |> String.split_on_char '~' 
    |> List.filter ( fun s -> s <> "")
    |> add_boundary_characters in 
  List.rev (add_boundary_characters (List.rev patternList)) |> convert_to_regex

(** [filter_list fields row i] is [row] with only the values that have the same
    index [i] as the [fields] that are [true]. *)
let filter_list fields row i = 
  List.filter (fun _ -> i := !i + 1; List.nth fields !i) row

(** [filter_pattern fields row ind pattern i operator] is [Some row] with only 
    the fields specified in [fields].  Returns the row if the row follows the SQL
    [pattern]. Returns [None] if the row does not follow the SQL [pattern] *)
let filter_pattern fields row ind pattern i operator = 
  if operator (List.nth row ind) pattern
  then Some (filter_list fields row i)
  else None

(** [match_pattern fields row ind where i] is [None] if [row] does not satisfy
    the [where] condition and [Some row] if it does. *)
let match_pattern fields row ind where i = 
  let partial_filter_pattern = filter_pattern fields row ind where.ptn i in
  match where.op with
  | Like ->
    if Str.string_match 
        (Str.regexp (parse_pattern where.ptn)) (List.nth row ind) 0
    then Some (filter_list fields row i)
    else None
  | EQ -> partial_filter_pattern (=)
  | NEQ -> partial_filter_pattern (<>)
  | GT -> partial_filter_pattern (>)
  | LT -> partial_filter_pattern (<)
  | GEQ -> partial_filter_pattern (>=)
  | LEQ -> partial_filter_pattern (<=)
  | s -> failwith "Expected a valid operator"

(** [filter_row schema fields where pattern row] is [Some row] with only the 
    fields specified in [fields]. Returns the row if [where] is [None] or if 
    [where] is not [None] and the row follows the SQL [pattern]. Returns [None] 
    if [where] is not [None] and the row does not follow the SQL [pattern]. *)
let filter_row schema fields (where: Query.where_obj option) row =
  let i = ref (-1) in 
  match where with 
  | None -> Some (filter_list fields row i)
  | Some where -> 
    check_fields schema where_msg [where.field];
    let ind = index where.field schema in
    match_pattern fields row ind where i

(** [filter_row_join table schema fields wehre row] is [None] if [where] is
    not [None] and [row] does not satisfy the where condition and [Some row] if
    [where] is [None] or if [where] is not [None] and [row] satisfies the where
    condition. *)
let filter_row_join table schema fields (where: Query.where_obj option) row =
  let i = ref (-1) in 
  match where with 
  | None -> Some (filter_list fields row i) 
  | Some where -> 
    let where_field = get_field where.field in 
    if fst where_field = table then 
      let ind = index (snd where_field) schema in
      check_fields schema select_msg [snd where_field];
      match_pattern fields row ind where i
    else Some (filter_list fields row i)

(** [filter_table fc schema fields where acc] is the table constructed
    from filtering the [fields] from each row of [fc]. *)
let rec filter_table fc (schema: string list) fields (where: where_obj option) acc = 
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
let order schema (qry_order: fieldname option) table = 
  match qry_order with 
  | None -> table
  | Some field -> 
    check_fields schema orderby_msg [field];
    List.sort (comp (index field schema)) table

(** [like_equal fc schema fields qry] is the OCaml table constructed from the
    rows in [fc] based on the "WHERE" condition in [qry]. Table only contains
    the fields specified in [fields] from the table [schema]. *)
let rec like_equal fc schema fields (qry_where: where_obj) = 
  filter_table fc schema fields (Some qry_where) []

(** [where tablename qry schema fields] is the OCaml table created from parsing 
    each row in the database table with [tablename]. Table results are filtered
    if there is a "WHERE" keyword in [qry]. Table only contains the fields
    specified in [fields] from the table [schema]. *)
let where tablename (qry_where: where_obj option) schema fields fc = 
  match qry_where with
  | None -> filter_table fc schema fields None [] 
  | Some w -> 
    check_fields schema where_msg [w.field]; 
    like_equal fc schema fields w

(** [order_join table schema qry table] is [table] with rows sorted by the the 
    field following the "ORDER BY" keyword in [qry]. *)
let order_join (qry: select_obj) (join: join_obj) (qry_order: fieldname option)
    schema fields table = 
  match qry_order with 
  | None -> table
  | Some field -> 
    let field' = get_field field in 
    if fst field' = qry.table then 
      begin 
        check_fields (fst schema) orderby_msg [snd field'];
        let comp_f = fst schema |> index (snd field') |> comp in 
        List.sort comp_f table
      end 
    else if fst field' = join.table then 
      begin 
        check_fields (snd schema) orderby_msg [snd field'];
        let comp_f = fst schema |> index (snd field') |> (+) (List.length fields)
        in 
        List.sort (comp comp_f) table
      end 
    else raise (Malformed "Must sort by a column in either table.") 

(** [get_cond field schema row] is the value at the index of [field] in 
    [schema]. *)
let get_cond field schema row = 
  let field' = get_field field |> snd in 
  check_fields schema join_msg [field'];
  index field' schema |> List.nth row

(** [join_filter_row qry table schema fields row] is [Some row] with [row]
    filtered to only contain the [fields] in [schema]. Returns [None] if there 
    is a [qry.where] is not [None] and [row] does not follow the where 
    pattern. *)
let join_filter_row (qry: select_obj) table schema fields row =
  let bool_fields = filter_fields schema fields [] in
  filter_row_join table schema bool_fields qry.where row

(** TODO: document *)
let rec rj_append_rows r acc (lst: string list option list) = 
  match lst with 
  | [] -> acc 
  | h::t -> begin 
      match h with 
      | None -> rj_append_rows r acc t
      | Some r' -> rj_append_rows r ((r'@r)::acc) t
    end 

(** TODO: document *)
let rec ilj_append_rows r acc (lst: string list option list) = 
  match lst with 
  | [] -> acc 
  | h::t -> begin 
      match h with 
      | None -> ilj_append_rows r acc t
      | Some r' -> ilj_append_rows r ((r@r')::acc) t
    end 

(** TODO: document *)
let rec append_rows' acc = function 
  | [] -> acc 
  | h::t -> append_rows' (h::acc) t

(** [populate_null acc row] is the fields selected for in [row] where each 
    value is [""]. *)
let rec populate_null acc = function 
  | [] -> acc 
  | h::t -> populate_null (""::acc) t

(** [check_where where schema fields row] is  *)
let check_where (where: where_obj option) schema fields row : string list option = 
  match where with 
  | None -> None
  | Some w -> 
    let i = ref (-1) in 
    let ind = index w.field schema in
    let bool_fields = filter_fields schema fields [] in
    match_pattern bool_fields row ind w i

(* RIGHT JOIN *)

(** TODO: document *)
let rec rj_filter_table qry (join: join_obj) cond ind schema1 fields fc1 acc = 
  try 
    let row = read_next_line fc1 in 
    if List.nth row ind = cond 
    then let row' = join_filter_row qry qry.table schema1 (fst fields) row in 
      rj_filter_table qry join cond ind schema1 fields fc1 (row'::acc)
    else rj_filter_table qry join cond ind schema1 fields fc1 acc 
  with 
  | exn -> 
    Stdlib.close_in fc1; 
    if acc <> [] then Some acc 
    else if qry.where = None then Some []
    else None 

(** TODO: document *)
let rj_row (qry: select_obj) join schema fields row : string list list option =
  let fc1 = get_in_chan qry.table in 
  let cond = get_cond (snd join.on) (snd schema) row in 
  let f_ind = (fst join.on |> get_field |> fst |> index) (fst schema) in 
  match rj_filter_table qry join cond f_ind (fst schema) fields fc1 [] with 
  | None -> begin 
      match check_where qry.where (fst schema) (fst fields) row with 
      | None -> None 
      | Some r' -> Some [((fst fields |> populate_null [])@r')]
    end 
  | Some r' when r' = [] -> begin 
      match join_filter_row qry qry.table (fst schema) (fst fields) row with 
      | None -> None 
      | Some r' -> Some [((snd fields |> populate_null [])@r')]
    end 
  | Some r -> begin 
      match join_filter_row qry join.table (snd schema) (snd fields) row with 
      | None -> None
      | Some r' -> Some (rj_append_rows r' [] r)
    end 

(** TODO: document *)
let rec right_join qry join schema fields fc acc = 
  let rows = try read_next_line fc |> rj_row qry join schema fields 
    with 
    | exn -> Stdlib.close_in fc; Some []
  in match rows with 
  | None -> right_join qry join schema fields fc acc
  | Some e when e = [] -> List.rev acc 
  | Some r ->
    let new_acc = append_rows' acc r in  
    right_join qry join schema fields fc new_acc

(* LEFT JOIN *)

(** TODO: document *)
let rec lj_filter_table qry (join: join_obj) cond ind schema2 fields fc1 acc = 
  try 
    let row = read_next_line fc1 in 
    if List.nth row ind = cond 
    then let row' = join_filter_row qry join.table schema2 (snd fields) row in 
      lj_filter_table qry join cond ind schema2 fields fc1 (row'::acc)
    else lj_filter_table qry join cond ind schema2 fields fc1 acc
  with 
  | exn -> 
    Stdlib.close_in fc1; 
    if acc <> [] then Some acc 
    else if qry.where = None then Some []
    else None 

(** TODO: document *)
let lj_row qry (join: join_obj) schema fields row : string list list option =
  let fc1 = get_in_chan join.table in 
  let cond = get_cond (fst join.on) (fst schema) row in 
  let f_ind = (snd join.on |> get_field |> snd |> index) (snd schema) in 
  match lj_filter_table qry join cond f_ind (snd schema) fields fc1 [] with 
  | None -> begin 
      match check_where qry.where (fst schema) (fst fields) row with 
      | None -> None 
      | Some r -> Some [(r@(snd fields |> populate_null []))]
    end  
  | Some r' when r' = [] -> begin 
      match join_filter_row qry qry.table (fst schema) (fst fields) row with 
      | None -> None 
      | Some r -> Some [(r@(snd fields |> populate_null []))]
    end
  | Some r' -> begin 
      match join_filter_row qry qry.table (fst schema) (fst fields) row with 
      | None -> None
      | Some r -> Some (ilj_append_rows r [] r')
      (* order is off but it works *)
    end 

(** TODO: document *)
let rec left_join qry join schema fields fc acc = 
  let rows = try read_next_line fc |> lj_row qry join schema fields 
    with 
    | exn -> Stdlib.close_in fc; Some []
  in match rows with 
  | None -> left_join qry join schema fields fc acc
  | Some e when e = [] -> List.rev acc 
  | Some r -> 
    let new_acc = append_rows' acc r in  
    left_join qry join schema fields fc new_acc

(* INNER JOIN *)

(** TODO: document *)
(** [filter_table2 qry_join cond f_index fc1] is...  *)
let rec ij_filter_table qry (join: join_obj) cond ind schema2 fields fc1 acc = 
  try 
    let row = read_next_line fc1 in 
    if List.nth row ind = cond 
    then 
      let row' = join_filter_row qry join.table schema2 (snd fields) row in 
      ij_filter_table qry join cond ind schema2 fields fc1 (row'::acc)
      (* join_filter_row qry join.table schema2 (snd fields) row *)
    else ij_filter_table qry join cond ind schema2 fields fc1 acc 
  with 
  | exn -> Stdlib.close_in fc1; 
    if acc <> [] then Some acc 
    else None

(** TODO: document *)
(** [join_row qry qry_join schema schema2 fields row] is...  *)
let ij_row qry (join: join_obj) schema fields row : string list list option =
  let fc1 = get_in_chan join.table in 
  let cond = get_cond (fst join.on) (fst schema) row in
  let f_ind = (snd join.on |> get_field |> snd |> index) (snd schema) in 
  match ij_filter_table qry join cond f_ind (snd schema) fields fc1 [] with 
  | None -> None
  | Some r' -> begin 
      match join_filter_row qry qry.table (fst schema) (fst fields) row with 
      | None -> None
      | Some r -> Some (ilj_append_rows r [] r')
    end 

(** TODO: document *)
(** [inner_join qry qry_join schema fields fc acc] is... *)
let rec inner_join qry join schema fields fc acc = 
  let row = try read_next_line fc |> ij_row qry join schema fields 
    with 
    | exn -> Stdlib.close_in fc; Some []
  in match row with 
  | None -> inner_join qry join schema fields fc acc
  | Some e when e = [] -> List.rev acc 
  | Some r -> 
    let new_acc = append_rows' acc r in  
    inner_join qry join schema fields fc new_acc

(** TODO: document *)
(** [join qry qry_join schema1 schema2 fields fc] is... 
    Raises [Malformed] if the query contains an invalid type of join. *)
let join (qry: select_obj) (join: join_obj) schema fields = 
  match join.join with 
  | Inner -> 
    let fc = get_in_chan qry.table in 
    inner_join qry join schema fields fc []
  | Left -> 
    let fc = get_in_chan qry.table in 
    left_join qry join schema fields fc []
  | Right -> 
    let fc = get_in_chan join.table in 
    right_join qry join schema fields fc []
  | None -> raise (Malformed "Must provide a type of join")

let select (qry: select_obj) =
  let tablename = qry.table in 
  let schema = table_schema (schema_from_txt ()) tablename in 
  match qry.join with 
  | None -> 
    let fc = get_in_chan tablename in 
    let fields = select_fields schema qry.fields in 
    let bool_fields = filter_fields schema fields [] in
    let table = where tablename qry.where schema bool_fields fc in 
    (order_fields schema bool_fields, order schema qry.order table)
  | Some join_obj -> 
    let schema1 = table_schema (schema_from_txt ()) join_obj.table in 
    let fields' = 
      select_fields_join qry.table join_obj.table (schema, schema1) qry.fields 
    in let table = join qry join_obj (schema, schema1) fields' in 
    (order_fields_join (schema, schema1) fields', 
     order_join qry join_obj qry.order (schema, schema1) (fst fields') table)

(* INSERT *)

(** TODO: document *)
let rec check_chars (l : string list) acc =
  match l with 
  | [] -> ()
  | h :: t -> 
    if String.contains h ':' 
    then raise (Malformed "Illegal character: ':'")
    else if String.contains h ')'
    then raise (Malformed "Illegal character: ')'")
    else if String.contains h '('
    then raise (Malformed "Illegal character: '('")
    else if String.contains h ','
    then raise (Malformed "Illegal character: ','")
    else check_chars t (h :: acc)

(** [vals_update sch cols vals acc] is the list of values to insert into a
    table with empty strings inserted for uninputted columns*)
let rec vals_update sch cols vals acc =
  match sch, cols with
  | [], [] -> List.rev acc
  | [], h :: t -> raise (Malformed "schema and columns do not match") 
  | h :: t, [] -> vals_update t cols vals ("" :: acc)
  | h1 :: t1, h2 :: t2 -> if h1 = h2 
    then vals_update t1 t2 (List.tl vals) (List.hd vals :: acc)
    else vals_update t1 (h2 :: t2) vals ("" :: acc)

let insert (qry: insert_obj) = 
  let schema = table_schema (schema_from_txt ()) qry.table in 
  check_chars qry.values [];
  match qry.fields with 
  | None -> if List.length qry.values <> List.length schema 
    then raise (Malformed "Values given do not match schema")
    else begin
      (* additional erro check*)

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

(** [delete_helper inc outc schema ind op ptn] iterates through the file in inc,
    evaluating the where condition and rewriting only those lines which do
    not satisfy the where condition*)
let rec delete_helper inc outc schema ind op ptn =
  try let line = read_next_line inc in
    if op (List.nth line ind) ptn
    then delete_helper inc outc schema ind op ptn
    else write_line outc line; 
    delete_helper inc outc schema ind op ptn
  with | End_of_file -> ()

(** [delete_helper_like inc outc schema ind ptn] iterates through the file in 
    inc, evaluating the like condition and rewriting only those lines which do
    not satisfy the like condition*)
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

let rec create_table (qry: create_obj) = 
  check_chars (qry.table :: qry.fields) [];
  let outc_schema = get_out_chan_schema () in
  write_line_table_schema outc_schema qry.table qry.fields; 
  close_out outc_schema;
  let outc_tables = get_out_chan qry.table in 
  close_out outc_tables

(** [drop_helper inc outc name] removes the line contaning the columns for
    the table with name [name]*)
let rec drop_helper inc outc schema name = 
  try let found_name, line = read_next_schema_line inc in
    if name = found_name
    then drop_helper inc outc schema name
    else write_line_table_schema outc (found_name) (line); 
    drop_helper inc outc schema name
  with | End_of_file -> ()

let drop_table qry = 
  let schema = table_schema (schema_from_txt ()) qry.table in 
  let outc_schema = get_out_chan_temp_schema () in
  let inc_schema = get_in_chan_schema () in
  output_string outc_schema (List.hd (read_next_line inc_schema) ^ "\n");
  drop_helper inc_schema outc_schema schema qry.table;
  close_out outc_schema;
  close_in inc_schema;
  Sys.remove (get_schema_path ());
  Sys.rename (get_schema_temp_path ()) (get_schema_path ());
  Sys.remove (get_path qry.table)

(** general help message for help command*)
let general_msg = 
  "'HELP' Command: \n"
  ^ "To get help for a command, type HELP and the command name \n"
  ^ "The list of valid commands is: SELECT, INSERT INTO, DELETE, TRUNCATE"
  ^ " TABLE, CREATE TABLE, DROP TABLE, CHANGE DATABASE, READ, HELP, and QUIT\n"

(** help message for select*)
let select_msg = 
  "'SELECT' QUERY: \n"
  ^ "'SELECT' selects specified fields from rows of a table \n"
  ^ "USAGE: 1) SELECT [fields] FROM [tablename]\n"
  ^ "       2) SELECT [fields] FROM [tablename] WHERE [b]\n"
  ^ "       3) SELECT [fields] FROM [tablename] WHERE [b] ORDER BY [c]\n"
  ^ "REQUIRES: [tablename] is the name of an existing table, [fields] is\n"
  ^ "'*' or valid fields in the schema seperated by commas, b is a "
  ^ "conditional following WHERE guidelines and c is a conditional following"
  ^ "ORDER BY guidelines.\n"

(** help message for insert*)
let insert_msg = 
  "'INSERT INTO' QUERY: \n"
  ^ "'INSERT INTO' inserts a new row into a table inserting data into the"
  ^ " specified coulmns.\n"
  ^ "USAGE: 1) INSERT INTO [tablename] (f1, f2,... fn) VALUES (v1, v2,... vn)\n"
  ^ "       2) INSERT INTO [tablename] VALUES (v1, v2,... vn)*\n"
  ^ "REQUIRES: [tablename] is the name of an existing table and" 
  ^ "(f1, f2,... fn) are colmum names in [tablename].\n"
  ^ "*For this case, length of (v1, v2,... vn) must match the number of"
  ^ " columns in the table.\n"

(** help message for delete*)
let delete_msg = 
  "'DELETE' QUERY: \n"
  ^ "'DELETE' deletes rows from a table, optionally meeting a given condition\n"
  ^ "USAGE: 1) DELETE FROM [tablename]\n"
  ^ "       2) DELETE FROM [tablename] WHERE [b]\n"
  ^ "REQUIRES: [tablename] is the name of an existing table and"
  ^ "b is a conditional following WHERE guidelines.\n"

(** help message for truncate*)
let truncate_msg = 
  "'TRUNCATE TABLE' QUERY: \n"
  ^ "'TRUNCATE TABLE' removes all data from a specified table.\n"
  ^ "USAGE: TRUNCATE TABLE [tablename]\n"
  ^ "REQUIRES: [tablename] is the name of an existing table.\n"

(** help message for create*)
let create_msg = 
  "'CREATE TABLE' QUERY: \n"
  ^ "'CREATE TABLE' creates a new, empty table with the specified name.\n"
  ^ "USAGE: CREATE TABLE [tablename] (colname1, colname2,... colnamen)\n "
  ^ "REQUIRES: [tablename] is a valid table name (no whitespace, commas, "
  ^ "parentheses, or colons).\n"
  ^ "oh? Create!\n"

(** help message for drop*)
let drop_msg = 
  "'DROP TABLE' QUERY: \n"
  ^ "'DROP TABLE' removes a specified table from the database.\n"
  ^ "USAGE: DROP TABLE [tablename]\n"
  ^ "REQUIRES: [tablename] is the name of an existing table.\n"

(** help message for change*)
let change_msg = 
  "'CHANGE DATABASE' Command: \n"
  ^ "'CHANGE DATABASE' changes the working database.\n"
  ^ "USAGE: CHANGE DATABASE [databasename]\n"
  ^ "REQUIRES: [databasename] is the name of an existing database visible to"
  ^ " the system.\n"

(** help message for read*)
let read_msg = 
  "'READ' Command: \n"
  ^ "'READ' reads queries from an input file and sends their results to an"
  ^ " output file\n"
  ^ "USAGE: READ FROM [filename]\n"
  ^ "REQUIRES: [filename] is the name of an existing file in the 'input' folder"
  ^ " containing properly formatted queries. Do not include the file extension."
  ^ "\n"

(** help message for quit*)
let quit_msg = 
  "'QUIT' Command: \n"
  ^ "'QUIT' exits the dbms\n"
  ^ "USAGE: 1) QUIT\n"
  ^ "       2) quit\n"
  ^ "       2) q\n"

(** help message for help*)
let help_msg = 
  "'HELP' Command: \n"

(** help message for where*)
let where_msg = 
  "'WHERE' Keyword: \n"
  ^ "'WHERE' specifies a condition under which a query performs its operation. "
  ^ " This dbms supports simple boolean operators and LIKE only.  AND and OR "
  ^ "are not supported.  Simple comparisons are done using Ocaml's Stlib "
  ^ "compare functions on strings.\n"  
  ^ "USAGE: WHERE [columnname] [op] [value]"
  ^ "SUPPORTED OPERATORS: =, <, >, <=, >=, !=, LIKE\n"

(** help message for join*)
let join_msg = 
  "'JOIN' Keyword: \n"

(** help message for order by*)
let order_msg = 
  "'ORDER BY' Keyword: \n"
  ^ "'ORDER BY' allows you to sort the output of a query in ascending order.\n"
  ^ "Note that all records are strings and numbers are sorted as strings.\n"
  ^ "For example '2' is greater than '12'."
  ^ "USAGE: ORDER BY [columnname]\n"
  ^ "REQUIRES: [columnanme] is the name of a column in the queried table\n"

(** help message for like*)
let like_msg = 
  "'LIKE' Operator: \n"
  ^ "The LIKE operator is used in a WHERE clause to search for a specified "
  ^ "pattern in a column.\n"
  ^ "There are two wildcards often used in conjunction with the LIKE operator\n"
  ^ "    1) % - The percent sign represents zero, one, or multiple characters\n"
  ^ "    2) _ - The underscore represents a single character\n"
  ^ "USAGE: WHERE [columnname] LIKE [pattern]\n"

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
  | "LIKE", _ -> like_msg
  | s1, s2 -> "Sorry, I can't help you with '" ^ s1 ^ " " ^ s2 ^ "'\n"