open Query
open Datardwt

(** [index field schema] is the index of [field] in list [schema]. *)
let index field schema =
  let i = ref (-1) in 
  List.fold_left 
    (fun ind x -> i := !i + 1; if x = field then !i else ind) 0 schema

let rec select_fields schema acc = function 
  | [] -> 
    raise Malformed
  | h::t when h = "FROM" -> 
    if acc = [] then raise Malformed 
    else List.rev acc
  | h::t when h = "*" -> schema
  | h::t -> select_fields schema (h::acc) t

let rec table_schema db_schema tablename = 
  match db_schema with 
  | [] -> 
    print_string "table schema";
    raise Malformed
  | h::t -> if fst h = tablename then snd h else table_schema t tablename

let rec select_table = function
  | [] -> raise Malformed
  | h::t when h = "FROM" -> 
    if t = [] then raise Malformed
    else List.hd t
  | h::t -> select_table t

(** [filter_fields fields acc schema] is a bool list [acc] where each elt 
    corresponds to a field in [schema], where the elt is [true] if the field 
    is in [fields] and false otherwise. *)
let rec filter_fields fields acc schema = 
  List.map (fun x -> if List.mem x schema then true else false) fields

(** [convert_to_regex pattern] is the SQL [pattern] converted to an OCaml 
    regex pattern. *)
let rec convert_to_regex = function
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

(** [filter_row schema fields where pattern row] is [Some row] with only the 
    fields specified in [fields]. Returns the row if [where] is [false] or if 
    [where] is [true] and the row follows the SQL [pattern]. Returns [None] if
    [where] is [true] and the row does not follow the SQL [pattern]. *)
let filter_row schema fields where (field, op, pattern) row =
  let i = ref (-1) in 
  if where then let ind = index field schema in
    match op with
    | s when s = "LIKE" ->
      if Str.string_match (Str.regexp (parse_pattern pattern)) (List.nth row ind) 0
      then Some (List.filter (fun _ -> i := !i + 1; List.nth fields !i) row)
      else None
    | s when s = "=" -> print_string (pattern);
      if (List.nth row ind) = pattern 
      then Some (List.filter (fun _ -> i := !i + 1; List.nth fields !i) row)
      else None 
    | s -> failwith "Expected LIKE or = after WHERE"

  else Some (List.filter (fun _ -> i := !i + 1; List.nth fields !i) row)

(** [filter_table fc schema fields where pattern acc] is the table constructed
    from filtering the [fields] from each row of [fc]. *)
let rec filter_table fc schema fields where p acc = 
  let row = try read_next_line fc |> filter_row schema fields where p with 
    | exn -> Stdlib.close_in fc; Some []
  in match row with 
  | None -> filter_table fc schema fields where p acc
  | Some e when e = [] -> List.rev acc 
  | Some r -> filter_table fc schema fields where p (r::acc)

(** [select_order qry] is None if the [qry] does not contain an "ORDER BY"
    command and Some of [field name] indicating the field the table should be
    sorted by otherwise. *)
let rec select_order = function 
  | [] -> None
  | o::b::t when o = "ORDER" && b = "BY" -> Some (List.hd t)
  | h::t -> select_order t

(** [comp n x y] is [-1] if the [n]th element of [x] is less than the [n]th value 
    of [y] using the Stdlib compare function; [0] if they are equal; and [1] if 
    the [n]th value of [x] is greater than the [n]th value of [y]. *)
let comp n x y = 
  let x' = List.nth x n in 
  let y' = List.nth y n in 
  Stdlib.compare x' y'

(** [order table schema qry table] is [table] with rows sorted by the the field
    following the "ORDER BY" keyword in [qry]. *)
let order schema qry table = 
  match select_order qry with 
  | None -> table
  | Some param -> List.sort (comp (index param schema)) table

(** [where_helper acc qry] is the (field name, operator, pattern) following 
    the keyword "WHERE" in [qry].
    Raises [Malformed] if [qry] is invalid. *)
let rec where_helper schema = function 
  | field::op::pattern::t when op = "=" || op = "LIKE" -> 
    if List.mem field schema then field, op, pattern
    else raise Malformed
  | _ -> raise Malformed

(** [select_where schema qry] is [None] if there is no "WHERE" keyword in [qry] 
    followed by a valid pattern and [Some param] where [param] is the 
    (field name, operator, pattern) and the operator is either "=" or "LIKE". *)
let rec select_where schema = function 
  | [] -> None
  | h::h'::t when h = "WHERE" && (h' <> "LIKE" && h' <> "=") -> 
    Some (where_helper schema t)
  | h::t -> select_where schema t 

(** [like_equal fc schema fields qry] is the OCaml table constructed from the
    rows in [fc] based on the "WHERE" condition in [qry]. Table only contains
    the fields specified in [fields] from the table [schema]. *)
let rec like_equal fc schema fields qry = 
  match qry with
  | [] -> raise Malformed
  | f::o::p::t when o = "=" || o = "LIKE" -> 
    filter_table fc schema fields true (f,o,p) []
  | h::t -> like_equal fc schema fields t

(** [where tablename qry schema fields] is the OCaml table created from parsing 
    each row in the database table with [tablename]. Table results are filtered
    if there is a "WHERE" keyword in [qry]. Table only contains the fields
    specified in [fields] from the table [schema]. *)
let where tablename qry schema fields = 
  let fc = get_in_chan tablename in 
  match select_where schema qry with
  | None -> filter_table fc schema fields false ("", "", "") [] 
  | Some param -> like_equal fc schema fields qry 

let select qry =
  let tablename = select_table qry in 
  let schema = table_schema (schema_from_txt ()) tablename in 
  let fields = select_fields schema [] qry in 
  let bool_fields = filter_fields schema [] fields in 
  let table = where tablename qry schema bool_fields in 
  (fields, order schema qry table)

(** TODO: document *)
(* Parses the table name form query*)
let insert_table qry =
  match qry with
  | [] -> raise Malformed
  | "INTO" :: t -> if List.length t < 2 then raise Malformed
    else List.hd t, List.tl t
  | h :: t -> raise Malformed

(** TODO: document *)
(* Parses the values from qry *)
let rec insert_vals qry acc_col acc_val = 
  match qry with
  | [] -> List.rev acc_col, List.rev acc_val
  | h :: t -> insert_vals t acc_col (h :: acc_val)

(** TODO: document *)
(* Parses the columns and values from qry *)
let rec insert_cols_and_vals qry acc_col acc_val =
  match qry with 
  | [] -> raise Malformed
  | "VALUES" :: t -> insert_vals t acc_col acc_val
  | h :: t -> insert_cols_and_vals t (h :: acc_col) acc_val

(** TODO: document *)
let rec vals_update sch cols vals acc =
  match sch, cols with
  | [], [] -> List.rev acc
  | [], h :: t -> raise Malformed
  | h :: t, [] -> vals_update t cols vals ("" :: acc)
  | h1 :: t1, h2 :: t2 -> if h1 = h2 
    then vals_update t1 t2 (List.tl vals) (List.hd vals :: acc)
    else vals_update t1 (h2 :: t2) vals ("" :: acc)

let insert qry = 

  let tablename, rest = insert_table qry in
  let (cols, vals) = insert_cols_and_vals rest [] [] in
  let schema = table_schema (schema_from_txt ()) tablename in 
  if cols = [] then 
    if List.length vals <> List.length schema then raise Malformed
    else 
      (* you have all cols so insert them all*)
      let outc = get_out_chan tablename in
      write_line outc vals; 
      close_out outc
  else 
    (* step through schema and vals, inserting empty strings where necessary,
       then write that*)
    let writable = vals_update schema cols vals [] in
    let outc = get_out_chan tablename in
    write_line outc writable; 
    close_out outc

(* Parses the table name form delete query*)
let delete_table qry =
  match qry with
  | [] -> raise Malformed
  | "FROM" :: t -> if List.length t < 2 then raise Malformed
    else List.hd t, List.tl t
  | h :: t -> raise Malformed

(* Parses the table name form delete query*)
let delete_table qry =
  match qry with
  | [] -> raise Malformed
  | "FROM" :: t -> if List.length t = 0 then raise Malformed
    else if List.length t < 2 then List.hd t, []
    else List.hd t, List.tl t
  | h :: t -> raise Malformed

let where_conditional lst =
  match List.tl lst with
  | col :: cond :: v :: [] -> begin
      match cond with
      | "=" -> col, (=), v
      | "!=" -> col, (<>), v
      | ">" -> col, (>), v
      | "<" -> col, (<), v
      | ">=" -> col, (>=), v
      | "<=" -> col, (<=), v
      | _ -> raise Malformed
    end
  | h :: t -> raise Malformed
  | [] -> raise Malformed

let rec find_col acc (col: string) sch : int =
  match sch with
  | [] -> -1
  | h :: t -> if h = col then acc
    else find_col (acc+1) col t

let rec get_col n lst : string =
  match lst with 
  | [] -> failwith "column not found"
  | h :: t -> if n = 0 then h else get_col (n-1) t

let rec delete_helper inc outc col_no (cond:'a->'a->bool) (v:string) =
  try let line = read_next_line inc in
    if cond (get_col col_no line) v
    then delete_helper inc outc col_no cond v
    else write_line outc line; 
    delete_helper inc outc col_no cond v
  with | End_of_file -> ()



let delete qry = 
  let tablename, rest = delete_table qry in
  (* Delete entire table *)
  if rest = [] then Sys.remove (get_path tablename)
  else 
    let temp_file = tablename ^ ".tmp" in
    let outc = get_out_chan temp_file in
    let inc = get_in_chan tablename in
    let col, cond, v = where_conditional rest in
    let schema = table_schema (schema_from_txt ()) tablename in 
    let col_no = find_col 0 col schema in
    if col_no = -1 then raise Malformed else begin
      delete_helper inc outc col_no cond v;
      close_in inc;
      close_out outc;
      Sys.remove (get_path tablename);
      Sys.rename (get_path temp_file) (get_path tablename)
    end

let join qry = 
  failwith "unimplemented"

(** [create_table_helper qry] is the table name and the string representation
    of the list of parameters specified in [qry].
    Raises [Malformed] if [qry] is invalid. *)
let rec create_table_helper = function 
  | [] -> raise Malformed
  | h::[] -> raise Malformed
  | h::t -> 
    let schema = List.fold_left (fun acc x -> acc^x^", ") (h^": ") t in 
    (h, String.sub schema 0 (String.length schema - 2))

let rec create_table qry = 
  let schema = create_table_helper qry in 
  let outc_schema = get_out_chan_schema in
  write_line outc_schema ([snd schema]); 
  close_out outc_schema;
  let outc_tables = get_out_chan (fst schema) in 
  close_out outc_tables;