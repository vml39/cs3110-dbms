open Query

(** [select qry] is the result of performing the select [qry] on the 
    database. *)
val select : Query.select_obj -> (string list * string list list)
(*
(** [select_table qry] is the table name parsed out from [qry]. 
    Raises [Malformed] if no table name provided in [qry] or if 
    illegal [qry] . *)
val select_table : string list -> string
*)

(** [table_schema dbs tb] is the list of fields in [tb] extracted
    from [dbs]. *)
val table_schema : (string * string list) list -> string -> string list 
(*
(** [select_fields schema acc qry] is the list of field name parsed out from 
    [qry] immediately following the "SELECT" keyword. Returns [schema] if "*" 
    follows "SELECT".
    Raises [Malformed] if no field names provided in [qry] or if 
    illegal [qry]. *)
val select_fields : string list -> string list -> string list -> string list
*)

(** TODO: document *)
val insert : Query.insert_obj -> unit

val delete : Query.delete_obj -> unit
(*
val join: string list -> string list list 
*)

(** [create_table qry] creates a file in the database's tables folder with the 
    parameters specified in [qry]. The table schema is also added to the 
    schema.txt file for the database. 
    Raises [Malformed] if no table name or fields provided in [qry]. *)
val create_table: string list -> unit

(** [delete_table qry] deletes a file in the database's tables folder with the 
    parameters specified in [qry]. The table schema is also removed to the 
    schema.txt file for the database. 
    Raises [Malformed] if no table name or fields provided in [qry]. *)
val delete_table: string list -> unit