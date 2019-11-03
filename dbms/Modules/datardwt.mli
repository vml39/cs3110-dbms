(** The abstract type representing a table *)
type t

(** The abstract type representa schema *)
type s

val schema_from_txt : t

(** [from_txt filename] is the table contained within the file filename
    Requires: filename is a valid filename contained in the directory*)
val table_from_txt : string -> t
