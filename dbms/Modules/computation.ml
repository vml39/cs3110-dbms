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

(** [filter_fields schema acc fields] is a bool list [acc] where each elt 
    corresponds to a field in [schema], where the elt is [true] if the field 
    is in [fields] and false otherwise. *)
let rec filter_fields fields acc schema = 
  List.map (fun x -> if List.mem x schema then true else false) fields

let rec convert_to_regex = function
  | [] -> ""
  | h::t when h = "%" -> ".*" ^ convert_to_regex t
  | h::t when h = "_" -> "." ^ convert_to_regex t
  | h::t -> h ^ convert_to_regex t

let parse_pattern pattern = 
  let patternList = 
    pattern
    |> Str.global_replace (Str.regexp "_") " _ " 
    |> Str.global_replace (Str.regexp "%") " % " 
    |> String.split_on_char ' ' 
    |> List.filter ( fun s -> s <> "") in 
  convert_to_regex patternList

(** TODO: document *)
let filter_row (schema:string list) fields where (field, op, pattern) row =
  let i = ref (-1) in 
  if where then let ind = index field schema in
    match op with
    | s when s = "LIKE" ->
      if Str.string_match (Str.regexp (parse_pattern pattern)) (List.nth row ind) 0
      then Some (List.filter (fun _ -> i := !i + 1; List.nth fields !i) row)
      else None
    | s when s = "=" -> 
      if (List.nth row ind) = pattern 
      then Some (List.filter (fun _ -> i := !i + 1; List.nth fields !i) row)
      else None 
    | s -> failwith "Expected LIKE or = after WHERE"

  else Some (List.filter (fun _ -> i := !i + 1; List.nth fields !i) row)

(** [filter_table schema acc table] is [table] with each row filtered to contain
    only the fields in [schema]. *)
(** TODO: document *)
let rec filter_table fc schema fields where p acc = 
  let row = try read_next_line fc |> filter_row schema fields where p with 
    | exn -> Stdlib.close_in fc; Some []
  in match row with 
  | None -> filter_table fc schema fields where p acc
  | Some e when e = [] -> acc 
  | Some r -> filter_table fc schema fields where p (r::acc)

(** [select_order qry] is None if the [qry] does not contain an "ORDER BY"
    command and Some of [field name] indicating the field the table should be
    sorted by otherwise. *)
let rec select_order = function 
  | [] -> None
  | o::b::t when o = "ORDER" && b = "BY" -> Some (List.hd t)
  | h::t -> select_order t

(** TODO: document *)
let comp n x y = 
  let x' = List.nth x n in 
  let y' = List.nth y n in 
  Stdlib.compare x' y'

(** [order table qry] is [table] with rows sorted by the the field following the
    "ORDER BY" keyword in [qry]. *)
let order schema qry table = 
  match select_order qry with 
  | None -> table
  | Some param -> List.sort (comp (index param schema)) table
(* compare only the field with the param *)

(** [where_helper acc qry] is [None] if the where [qry] is malformed and 
    [Some param] where [param] is the condition to filter the rows in the
    table by otherwise. *)
(** TODO: update docs *)
let rec where_helper schema = function 
  | field::op::pattern::t -> 
    if List.mem field schema then Some (field, op, pattern)
    else raise Malformed
  | _ -> raise Malformed

(** [select_where acc qry] is [None] if there is no where keyword in [qry] 
    and [Some param] where [param] is the condition to filter the rows in the
    table by otherwise. *)
let rec select_where schema = function 
  | [] -> None
  | h::t when h = "WHERE" -> where_helper schema t
  | h::t -> select_where schema t 

(** TODO: document *)
let rec like_equal fc schema fields qry  : string list list = 
  match qry with
  | [] -> raise Malformed
  | f::o::p::t when o = "=" || o = "LIKE" -> filter_table fc schema fields true (f,o,p) []
  | h::t -> like_equal fc schema fields t

(** [where qry schema row] is [row] filtered by the fields selected for in [qry] and where
    fields follow the condition specified after "WHERE" in [qry]. *)
let where tablename qry schema fields = 
  let file_channel = get_in_chan tablename in 
  match select_where schema qry with
  | None -> filter_table file_channel schema fields false ("", "", "") [] 
  | Some param -> like_equal file_channel schema fields qry 

let select qry =
  let tablename = select_table qry in 
  let schema = table_schema (schema_from_txt ()) tablename in 
  let fields = select_fields schema [] qry in 
  let bool_fields = filter_fields schema [] fields in 
  let table = where tablename qry schema bool_fields in 
  (fields, order schema qry table)

(* Parses the table name form query*)
let insert_table qry =
  match qry with
  | [] -> raise Malformed
  | "INTO" :: t -> List.hd t, List.tl t
  | h :: t -> raise Malformed

(* Parses the values from qry *)
let rec insert_vals qry acc_col acc_val = 
  match qry with
  | [] -> acc_col, acc_val
  | h :: t -> insert_vals t acc_col (h :: acc_val)

(* Parses the columns and values from qry *)
let rec insert_cols_and_vals qry acc_col acc_val =
  match qry with 
  | [] -> raise Malformed
  | "VALUES" :: t -> insert_vals t acc_col acc_val
  | h :: t -> insert_cols_and_vals t (h :: acc_col) acc_val

let rec vals_update sch cols vals acc =
  match sch, cols with
  | [], [] -> acc
  | [], h :: t -> raise Malformed
  | h :: t, [] -> vals_update t cols vals ("" :: acc)
  | h1 :: t1, h2 :: t2 -> if h1 = h2 
    then vals_update t1 t2 (List.tl vals) (List.hd vals :: acc)
    else vals_update t1 (h2 :: t2) vals ("" :: acc)

let insert qry = 
  let tablename, rest = insert_table qry in
  let (cols, vals) = insert_cols_and_vals rest [] [] in
  let schema = table_schema (schema_from_txt ()) tablename in 
  if vals = [] then 
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

let delete qry = 
  failwith "unimplemented"

let join qry = 
  failwith "unimplemented"

(** TODO: document *)
let rec create_table_helper = function 
  | [] -> raise Malformed
  | h::[] -> raise Malformed
  | h::t -> 
    let schema = List.fold_left (fun acc x -> acc^x^", ") (h^": ") t in 
    (h, String.sub schema 0 (String.length schema - 2))

(** TODO: document *)
let rec create_table qry = 
  let schema = create_table_helper qry in 
  (* @Robert 
     schema is a string * string where the first string is the table name and the 
     second string is the table schema in the same format as schema.txt 
     create a file in tables with table name and add schema line to schema.txt
  *)
  failwith "unimplemented"