open Query
open Datardwt

let like param = 
  failwith "unimplemented"

(** [where_helper acc qry] is [None] if the where [qry] is malformed and 
    [Some param] where [param] is the condition to filter the rows in the
    table by otherwise. *)
let rec where_helper acc = function 
  | [] -> 
    if acc = [] then None
    else Some (List.rev acc)
  | h::t when h = "LIKE" -> 
    if acc = [] then None
    else Some (List.rev acc)
  | h::t -> where_helper (h::acc) t

(** [select_where acc qry] is [None] if there is no where keyword in [qry] 
    and [Some param] where [param] is the condition to filter the rows in the
    table by otherwise. *)
let rec select_where = function 
  | [] -> None
  | h::t when h = "WHERE" -> where_helper [] t
  | h::t -> select_where t 

(** [where qry] is *)
let where qry = 
  match select_where qry with
  | None -> 
    fun lst -> lst 
  | Some param -> 
    fun lst -> List.filter (fun fd -> fd = param)
(* the field is equal to the param set *)
(* failwith "unimplemented" *)

let rec select_fields acc = function 
  | [] -> raise Malformed
  | h::t when h = "FROM" -> 
    if acc = [] then raise Malformed 
    else List.rev acc
  | h::t -> select_fields (h::acc) t

let rec table_schema db_schema tablename = 
  match db_schema with 
  | [] -> raise Malformed
  | h::t -> if fst h = tablename then snd h else table_schema t tablename

let rec select_table = function
  | [] -> raise Malformed
  | h::t when h = "FROM" -> 
    if t = [] then raise Malformed
    else List.hd t
  | h::t -> select_table t

(** [select_order qry] is None if the [qry] does not contain an "ORDER BY"
    command and Some of [field name] indicating the field the table should be
    sorted by otherwise. *)
let rec select_order = function 
  | [] -> None
  | h::h'::t when h = "ORDER" && h' = "BY" -> Some (List.hd t)
  (* what if there's a space? need to parse further *)
  | h::t -> select_order t

(** [order table field] is [table] with rows sorted by the [field]. *)
let rec order table field = 
  failwith "unimplemented"

(* need a compare function *)
(** [order table qry] is [table] with rows sorted by the the field following the
    "ORDER BY" keyword in [qry]. *)
let order qry = 
  (* match select_order qry with 
     | None -> 
     fun lst -> lst 
     | Some param -> 
     fun lst -> List.sort lst  *)
  failwith "unimplemented"

(** [filter_fields schema acc fields] is a bool list [acc] where each elt 
    corresponds to a field in [schema], where the elt is [true] if the field 
    is in [fields] and false otherwise. *)
let rec filter_fields fields acc schema = 
  if List.nth schema 0 = "*" then List.map (fun _ -> true) fields
  else List.map (fun x -> if List.mem x schema then true else false) fields

let filter_row i schema acc row =
  List.filter (fun _ -> i := !i + 1; List.nth schema !i) row

(** [filter_table schema acc table] is [table] with each row filtered to contain
    only the fields in [schema]. *)
let rec filter_table schema acc = function
  | [] -> List.rev acc 
  | h::t -> 
    let i = ref (-1) in 
    let filtered_row = filter_row i schema acc h in
    filter_table schema (filtered_row::acc) t

let select qry =
  let schema = table_schema (schema_from_txt ()) (select_table qry) in 
  let fields = select_fields [] qry |> filter_fields schema [] in 
  let table = 
    table_from_txt (select_table qry) |> filter_table fields [] in
  (* let order_by = select_order qry in 
     match order_by with 
     | None -> table
     | Some field -> order table order_by *)
  let i = ref(-1) in
  let fields = filter_row i fields [] schema in
  (schema, fields, table)

let insert qry = 
  failwith "unimplemented"

let delete qry = 
  failwith "unimplemented"

let join qry = 
  failwith "unimplemented"