(** Parsing of user queries. *)

(**The type representing a line in a record. *)
type fieldname = string

(** The type representing a table in a databse. *)
type tablename = string

(** The type representing a pattern used in WHERE queries. *)
type pattern = string

(** The type representing the operator used in WHERE queries. *)
type operator = | EQ | GT | LT | GEQ | LEQ | Like | NEQ | None

(** The type representing different types of joins. *)
type join_type = 
  | Inner | Left | Right | None

(** The type representing a parsed WHERE query. *)
type where_obj = {
  field: fieldname;
  op: operator;
  ptn: pattern
}

(** The type representing a parsed JOIN query. *)
type join_obj = {
  table: tablename;
  join: join_type;
  on: fieldname * fieldname;
}

(** The type representing a parsd SELECT query. *)
type select_obj = {
  table: tablename; 
  fields: fieldname list; 
  where: where_obj option; 
  order: fieldname option;
  join: join_obj option
}

(** The type representing a parsed INSERT query. *)
type insert_obj = {
  table: tablename;
  fields: fieldname list option;
  values: string list
}

(** The type representing a parsed DELETE query.  *)
type delete_obj = {
  table: tablename;
  where: where_obj option
}

(** The type representing a parsed CREATE query.  *)
type create_obj = {
  table: tablename;
  fields: fieldname list;
}

(** The type representing a parsed DROP query.  *)
type drop_obj = {
  table: tablename;
}

(** The type representing a parsed HELP query.  *)
type help_obj = {
  s1: string;
  s2: string;
}

(** The type representing a parsed query *)
type t = 
  | Select of select_obj
  | Insert of insert_obj
  | Delete of delete_obj
  | Create of create_obj
  | Drop of drop_obj
  | Read of string 
  | Changedb of string
  | Help of help_obj
  | Quit

(** Raised when an empty query is parsed. *)
exception Empty

(** Raised when a malformed query is encountered. *)
exception Malformed of string

(** [parse str] parses a user's input into a query of type t

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9 (English
    characters only)), commas, and space characters (only ASCII character code 
    32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the query is malformed. A query
    is {i malformed} if the query does not follow guidelines in the HELP command
*)
val parse : string -> t