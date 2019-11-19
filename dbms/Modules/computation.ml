open Query
open Datardwt

let like_equal fc schema acc (field, op, pattern) = 
  match op with
  | s when s = "LIKE" ->
  | s when s = "=" -> filter_fields

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

(** [filter_fields schema acc fields] is a bool list [acc] where each elt 
    corresponds to a field in [schema], where the elt is [true] if the field 
    is in [fields] and false otherwise. *)
let rec filter_fields fields acc schema = 
  if List.nth schema 0 = "*" then List.map (fun _ -> true) fields
  else List.map (fun x -> if List.mem x schema then true else false) fields

(** TODO: document *)
let filter_row schema row =
  let i = ref (-1) in 
  List.filter (fun _ -> i := !i + 1; List.nth schema !i) row

(** [filter_table schema acc table] is [table] with each row filtered to contain
    only the fields in [schema]. *)
let rec filter_table fc schema acc = 
  let row = try next_line fc |> filter_row schema with 
    | exn -> Stdlib.close_in fc; []
  in if row = [] then acc else filter_table fc schema (row::acc)

(** [select_order qry] is None if the [qry] does not contain an "ORDER BY"
    command and Some of [field name] indicating the field the table should be
    sorted by otherwise. *)
let rec select_order = function 
  | [] -> None
  | h::h'::t when h = "ORDER" && h' = "BY" -> Some (List.hd t)
  (* what if there's a space? need to parse further *)
  | h::t -> select_order t

let comp param x y = 
  (* get the field from x, y*)
  (* let x' = List.filter (fun _ -> i := !i + 1; List.nth schema !i) row
     let y' =
     match Stdlib.compare x' y' with
     | x when x'<0 -> -1
     | 0 -> 0
     | _ -> 1 *)
  failwith "unimplemented"

(* need a compare function *)
(** [order table qry] is [table] with rows sorted by the the field following the
    "ORDER BY" keyword in [qry]. *)
let order qry = 
  match select_order qry with 
  | None -> fun lst -> lst 
  | Some param -> 
    fun lst -> List.sort (comp param) lst
(* compare only the field with the param *)

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

(** [where qry schema row] is [row] filtered by the fields selected for in [qry] and where
    fields follow the condition specified after "WHERE" in [qry]. *)
let where tablename qry schema = 
  let file_channel = get_file_chan tablename in 
  match select_where qry with
  | None -> filter_table file_channel schema []
  | Some param -> 
    (* if the param matches the where cond, add this row else don't *)
    (* if fd = param then Some filter_row i schema acc h else None *)
    failwith "unimplemented where"
(* the field is equal to the param set *)

let select qry =
  let tablename = select_table qry in 
  let schema = table_schema (schema_from_txt ()) tablename in 
  let fields = select_fields [] qry |> filter_fields schema [] in 
  (schema, where tablename qry fields)

(* need to modify table here *)

(* table_from_txt (select_table qry) |> filter_table fields [] in *)
(* let order_by = select_order qry in 
   match order_by with 
   | None -> table
   | Some field -> order table order_by *)
(* let i = ref(-1) in
   let fields = filter_row i fields [] schema in
   (fields, table) *)

(* inside fun that processes query, helper function that does it line by line (recursive)
   call get file channel w table name, pass into rec function that reads single lines 
   try next_line with..., then return accumulator *)

let insert qry = 
  failwith "unimplemented"

let delete qry = 
  failwith "unimplemented"

let join qry = 
  failwith "unimplemented"