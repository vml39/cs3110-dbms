val where: 'a -> 'b

val like : 'a  -> 'b

val order : 'a -> 'b

val sort : 'a -> 'b

val select : string list -> string list list

val select_fields : string list -> string list -> string list

val select_table : string list -> string

val insert : string list -> unit

val delete : string list -> unit

val join: string list -> string list list 
