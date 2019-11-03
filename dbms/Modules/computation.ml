open Queries

(* param is a string list *)
let where param = 
  failwith "unimplemented"

let like param = 
  failwith "unimplemented"

let order param = 
  failwith "unimplemented"

let sort param = 
  failwith "unimplemented"

let rec select_fields acc = function 
  | [] -> raise Malformed
  | h::t when h = "FROM" -> List.rev acc
  | h::t -> h::acc

let rec select_table = function
  | [] -> raise Malformed
  | h::t when h = "FROM" -> List.hd t
  | h::t -> select_table t

(* gets the output from the datardwt function *)
let select qry =
  failwith "unimplemented"

let insert qry = 
  failwith "unimplemented"

let delete qry = 
  failwith "unimplemented"

let join qry = 
  failwith "unimplemented"