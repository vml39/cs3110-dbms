(** The abstract type representing a table *)
type t = (string list) list

(** The abstract type representa schema *)
type s = (string * (string list)) list

val schema_from_txt : t

(** [from_txt filename] is the table contained within the file filename
    Requires: filename is a valid filename contained in the directory*)
val table_from_txt : string -> t
