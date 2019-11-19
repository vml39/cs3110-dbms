open Query
open Datardwt

(** TODO: document *)
let index field schema =
  let i = ref (-1) in 
  List.fold_left 
    (fun ind x -> i := !i + 1; if x = field then !i else ind) 0 schema

let like param = 
  failwith "unimplemented"

let rec select_fields acc = function 
  | [] -> 
    print_string "select fields";
    raise Malformed
  | h::t when h = "FROM" -> 
    if acc = [] then raise Malformed 
    else List.rev acc
  | h::t -> select_fields (h::acc) t

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
  if List.nth schema 0 = "*" then List.map (fun _ -> true) fields
  else List.map (fun x -> if List.mem x schema then true else false) fields

(** TODO: document *)
let filter_row schema where (op, pattern) row =
  let i = ref (-1) in 
  if where then failwith "unimplemented where"
  else List.filter (fun _ -> i := !i + 1; List.nth schema !i) row

(** [filter_table schema acc table] is [table] with each row filtered to contain
    only the fields in [schema]. *)
let rec filter_table fc schema where p acc = 
  let row = try read_next_line fc |> filter_row schema where p with 
    | exn -> Stdlib.close_in fc; []
  in if row = [] then acc else filter_table fc schema where p (row::acc)

(** [select_order qry] is None if the [qry] does not contain an "ORDER BY"
    command and Some of [field name] indicating the field the table should be
    sorted by otherwise. *)
let rec select_order = function 
  | [] -> None
  | o::s::b::t when o = "ORDER" && s = " " && b = "BY" -> Some (List.hd t)
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

let like_equal fc schema acc (field, op, pattern) : string list list = 
  (* let index = field schema in 
     match op with
     | s when s = "LIKE" -> 
     let row = try read_next_line fc |> (* Check if pattern matches *)
     | s when s = "=" -> filter_fields *)
  failwith "unimplemented"

(** [where qry schema row] is [row] filtered by the fields selected for in [qry] and where
    fields follow the condition specified after "WHERE" in [qry]. *)
let where tablename qry schema fields = 
  let file_channel = get_in_chan tablename in 
  match select_where schema qry with
  | None -> filter_table file_channel fields false (None, None) [] 
  | Some param -> like_equal file_channel schema [] param 
(* if the param matches the where cond, add this row else don't *)
(* if fd = param then Some filter_row i schema acc h else None *)
(* the field is equal to the param set *)

let select qry =
  let tablename = select_table qry in 
  let schema = table_schema (schema_from_txt ()) tablename in 
  let fields = select_fields [] qry |> filter_fields schema [] in 
  let table = where tablename qry schema fields in 
  (schema, order schema qry table)

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