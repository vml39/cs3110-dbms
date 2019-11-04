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

(** [order table field] is [table] with rows sorted by the [field]. *)
let rec order table field = 
  failwith "unimplemented"

(** [select_fields acc qry] is the list of field names [acc] directly 
    following the "SELECT" keyword in a [qry]. *)
let rec select_fields acc = function 
  | [] -> raise Malformed
  | h::t when h = "FROM" -> 
    if acc = [] then raise Malformed 
    else List.rev acc
  | h::t -> h::acc

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

(* need a compare function *)
let order qry = 
  (* match select_order qry with 
  | None -> 
    fun lst -> lst 
  | Some param -> 
    fun lst -> List.sort lst  *)
  failwith "unimplemented"

(**[filter_fields schema acc qry] is [qry] *)
(* take in schema, construct bool list and correspond if field should be selected *)
let rec filter_fields schema acc = function 
  | [] -> List.rev acc
  | h::t -> filter_fields schema acc t
(* if this field correponds with schema field, then add to acc *)

(* take bool list and filter each row based on true/false *)
let rec filter_table schema acc = 
(* function *)
  (* | [] -> List.rev acc 
  | h::t -> 
    filter_table schema (filter_fields schema [] h)::acc t
  | _ ->  *)
  failwith "unimplemented"

(* get table
   filter table by field name --> consider where clause
   order table
    *)
let select qry =
  let table = 
     table_from_txt (select_table qry) |> filter_table schema_from_txt [] in
     (* let order_by = select_order qry in 
     match order_by with 
     | None -> table
     | Some field -> order table order_by *)
  (* failwith "unimplemented" *)

let insert qry = 
  failwith "unimplemented"

let delete qry = 
  failwith "unimplemented"

let join qry = 
  failwith "unimplemented"