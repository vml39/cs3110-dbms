open Queries
open Datardwt

let like param = 
  failwith "unimplemented"

(*  *)
let where param = 
  failwith "unimplemented"

(* gets the contents of the where clause *)
let rec where_helper acc = function 
  | [] -> where (List.rev acc)
  | h::t when h = "WHERE" -> where_helper (h::acc) t
  | h::t when h = "LIKE" -> like t
  | h::t -> where_helper acc t 

(** [order table field] is [table] with rows sorted by the [field]. *)
let rec order table field = 
  failwith "unimplemented"

(** [select_fields acc qry] is the list of field names [acc] directly 
    following the "SELECT" keyword in a [qry]. *)
let rec select_fields acc = function 
  | [] -> raise Malformed
  | h::t when h = "FROM" -> List.rev acc
  | h::t -> h::acc

let rec select_table = function
  | [] -> raise Malformed
  | h::t when h = "FROM" -> List.hd t
  | h::t -> select_table t

(** [select_order qry] is None if the [qry] does not contain an "ORDER BY"
    command and Some of [field name] indicating the field the table should be
    sorted by otherwise. *)
let rec select_order = function 
  | [] -> None
  | h::h'::t when h = "ORDER" && h' = "BY" -> Some (List.hd t)
  | h::t -> select_order t

let rec filter_fields schema acc = function 
  | [] -> List.rev acc
  | h::t -> filter_fields schema acc t
  (* if this field correponds with schema field, then add to acc *)

(* only get the specific schema requirements *)
let rec filter_table schema acc = 
  (* function *)
  (* | [] -> List.rev acc 
  | h::t -> filter_table schema (filter_fields schema [] h)::acc t *)
  failwith "unimplemented"

(* get table
   filter table by field name --> consider where clause
   order table
    *)
let select qry =
  (* let table = 
    table_from_txt (select_table qry) |> filter_table schema_from_txt [] in
  let order_by = select_order qry in 
  match order_by with 
  | None -> table
  | Some field -> order table order_by *)
  failwith "unimplemented"

let insert qry = 
  failwith "unimplemented"

let delete qry = 
  failwith "unimplemented"

let join qry = 
  failwith "unimplemented"