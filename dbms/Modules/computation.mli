(** [select] is the *)
val select : string list -> string list list

(** [select_table qry] is the table name parsed out from the [qry]. *)
val select_table : string list -> string

val select_fields : string list -> string list -> string list

val insert : string list -> unit

val delete : string list -> unit

val join: string list -> string list list 
