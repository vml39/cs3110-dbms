(** Computation of parsed queries*)
open Query

(** [select qry] is the result of performing the select [qry] on the 
    database. *)
val select : Query.select_obj -> (string list * string list list)

(** [table_schema dbs tb] is the list of fields in [tb] extracted
    from [dbs]. *)
val table_schema : (string * string list) list -> string -> string list 

(** [insert qry] inserts the values speciefied in qry 
    into the columns specified in query into table in qry*)
val insert : Query.insert_obj -> unit

(** [delete qry] deletes either all rows in a table or rows matchin a 
    where condition given in qry*)
val delete : Query.delete_obj -> unit

(** [create_table qry] creates a file in the database's tables folder with the 
    parameters specified in [qry]. The table schema is also added to the 
    schema.txt file for the database. 
    Raises [Malformed] if no table name or fields provided in [qry]. *)
val create_table: Query.create_obj -> unit

(** [drop_table qry] deletes a file in the database's tables folder with the 
    name specified in [qry]. The table's schema is also removed from the 
    schema.txt file for the database. 
    Raises [Malformed] if no table name or fields provided in [qry]. *)
val drop_table: Query.drop_obj -> unit

(** [help_with qry] Is a string with a help message pertaining to the 
    input qry**)
val help_with: Query.help_obj -> string