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

(** [get_file_chan filename] is the in_channel object for file filename
    Requires: filename is a valid filename contained in the directory*)
val get_file_chan : string -> in_channel

(** [next_line fc] is the next line from the table in file filename
    Requires: fc is a valid file channel of a file contained in the directory*)
val next_line : in_channel -> l