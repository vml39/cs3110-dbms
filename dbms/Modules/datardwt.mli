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

(** [get_in_chan filename] is the in_channel object for file filename
    Requires: filename is a valid filename contained in the directory*)
val get_in_chan : string -> in_channel

(** [get_out_chan filename] is the out_channel object for file filename
    Requires: filename is a valid filename contained in the directory*)
val get_out_chan : string -> out_channel

(** [next_line fc] is the next line from the table in file filename
    Requires: fc is a valid file channel of a file contained in the directory
    Raises: End_of_file if at the end of the file*)
val read_next_line : in_channel -> l

(** [write_line fc lst] appends the contents of lst as a new line to the table 
    in fc 
    Requires: fc is a valid file channel of a file contained in the directory
*)
val write_line : out_channel -> string list -> unit

(*
val write_to_schema : out_channel -> string list -> unit

val create_file : out_channel -> unit *)