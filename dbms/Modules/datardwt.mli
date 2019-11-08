(** The abstract type representing a line in a table *)
type l = string list

(** The abstract type representing a table *)
type t = (string list) list

(** The abstract type representa schema *)
type s = (string * (string list)) list

(** [schema_from_txt] is as association list representing the schema for
    the current database
    Requires: filename is a valid filename contained in the directory*)
val schema_from_txt : unit -> s

(** [table_from_txt filename] is the table contained within the file filename
    Requires: filename is a valid filename contained in the directory*)
val table_from_txt : string -> t

(** [next_line filename] is the next line from the table in file filename
    Requires: filename is a valid filename contained in the directory*)
val next_line : string -> l