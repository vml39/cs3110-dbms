open Query
open Datardwt

(** [index field schema] is the index of [field] in list [schema]. *)
let index field schema =
  let i = ref (-1) in 
  List.fold_left 
    (fun ind x -> i := !i + 1; if x = field then !i else ind) 0 schema

(** TODO: document *)
let rec check_fields schema fields = function 
  | [] -> fields 
  | h::t -> 
    if List.mem h schema then check_fields schema fields t 
    else raise (Malformed "Field selected not in schema")

(** TODO: document *)
let select_fields schema fields = 
  if fields = ["*"] then schema
  else check_fields schema fields fields 

(** TODO: document *)
let rec print_fields schema bfields acc = 
  match schema, bfields with 
  | [], [] -> List.rev acc
  | h::t, h'::t' -> 
    if h' then print_fields t t' (h::acc) 
    else print_fields t t' acc
  | _ -> failwith "inequal num of fields"

let rec table_schema db_schema tablename = 
  match db_schema with 
  | [] -> 
    raise (Malformed "Table schema is empty")
  | h::t -> if fst h = tablename then snd h else table_schema t tablename

(** [filter_fields fields acc schema] is a bool list [acc] where each elt 
    corresponds to a field in [schema], where the elt is [true] if the field 
    is in [fields] and false otherwise. *)
let rec filter_fields fields acc schema = 
  List.map (fun x -> if List.mem x schema then true else false) fields

(** TODO: document *)
let rec order_fields schema fields = 
  let i = ref (-1) in
  List.filter(fun x -> i := !i + 1; List.nth fields !i) schema

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

(** [filter_row schema fields where pattern row] is [Some row] with only the 
    fields specified in [fields]. Returns the row if [where] is [false] or if 
    [where] is [true] and the row follows the SQL [pattern]. Returns [None] if
    [where] is [true] and the row does not follow the SQL [pattern]. *)
let filter_row schema fields where (field, op, pattern) row =
  let i = ref (-1) in 
  if where then let ind = index field schema in
    match op with
    | Like ->
      if Str.string_match (Str.regexp (parse_pattern pattern)) (List.nth row ind) 0
      then Some (List.filter (fun _ -> i := !i + 1; List.nth fields !i) row)
      else None
    | EQ -> 
      if (List.nth row ind) = pattern 
      then Some (List.filter (fun _ -> i := !i + 1; List.nth fields !i) row)
      else None 
    | s -> failwith "Expected LIKE or = after WHERE"

  else Some (List.filter (fun _ -> i := !i + 1; List.nth fields !i) row)

(** [filter_table fc schema fields where pattern acc] is the table constructed
    from filtering the [fields] from each row of [fc]. *)
let rec filter_table fc (schema: string list) fields where p acc = 
  let row = try read_next_line fc |> filter_row schema fields where p with 
    | exn -> Stdlib.close_in fc; Some []
  in match row with 
  | None -> filter_table fc schema fields where p acc
  | Some e when e = [] -> List.rev acc 
  | Some r -> filter_table fc schema fields where p (r::acc)

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
  filter_table fc schema fields true 
    (qry_where.field, qry_where.op, qry_where.ptn) []

(** [where tablename qry schema fields] is the OCaml table created from parsing 
    each row in the database table with [tablename]. Table results are filtered
    if there is a "WHERE" keyword in [qry]. Table only contains the fields
    specified in [fields] from the table [schema]. *)
let where tablename (qry_where : Query.where_obj option) schema fields = 
  let fc = get_in_chan tablename in 
  match qry_where with
  | None -> filter_table fc schema fields false ("", None, "") [] 
  | Some w -> like_equal fc schema fields w

(* check if fields in schema *)

let select (qry : Query.select_obj) =
  let tablename = qry.table in 
  let schema = table_schema (schema_from_txt ()) tablename in 
  let fields = select_fields schema qry.fields in 
  let bool_fields = filter_fields schema [] fields in 
  let table = where tablename qry.where schema bool_fields in 
  (order_fields schema bool_fields, order schema qry.order table)

(*
(** TODO: document *)
(* Parses the table name form query*)
let insert_table (qry : Query.insert_obj) =
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
*)
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
(*
(* Parses the table name form delete query*)
let delete_table qry =
  match qry with
  | [] -> raise Malformed
  | "FROM" :: t -> if List.length t = 0 then raise Malformed
    else if List.length t = 1 then List.hd t, []
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
*)
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
      let schema = table_schema (schema_from_txt ()) qry.table in ()
(*
      match where_rec.ptn with
      | EQ -> 
      | GT ->
      | LT ->
      | GEQ ->
      | LEQ ->
      | Like ->
      | NEQ ->
      | None ->

        let col, cond, v = where_conditional rest in

        let col_no = find_col 0 col schema in
        if col_no = -1 then raise Malformed else begin
          delete_helper inc outc col_no cond v;
          close_in inc;
          close_out outc;
          Sys.remove (get_path tablename);
          Sys.rename (get_path temp_file) (get_path tablename)
          *)
    end

let join qry = 
  failwith "unimplemented"

let rec create_table (qry: Query.create_obj) = 
  let outc_schema = get_out_chan_schema in
  write_line_schema outc_schema qry.fields; 
  close_out outc_schema;
  let outc_tables = get_out_chan qry.table in 
  close_out outc_tables

let delete_table qry = 
  failwith "unimplemented" 