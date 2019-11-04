(** [select qry] is the result of performing the select [qry] on the 
    database. *)
val select : string list -> string list list

(** [select_table qry] is the table name parsed out from [qry]. 
    Raises [Malformed] if no table name provided in [qry] or if 
    illegal [qry] . *)
val select_table : string list -> string

(** [select_table qry] is the list of field name parsed out from [qry]. 
    Raises [Malformed] if no field names provided in [qry] or if 
    illegal [qry]. *)
val select_fields : string list -> string list -> string list

val insert : string list -> unit

val delete : string list -> unit

val join: string list -> string list list 
