(* Parsing of user queries. *)

type fieldname = string
type tablename = string
type pattern = string
type operator = | EQ | GT | LT | GEQ | LEQ | Like | NEQ | None

type where_obj = {
  field: fieldname;
  op: operator;
  ptn: pattern
}

type select_obj = {
  table: tablename; 
  fields: fieldname list; 
  where: where_obj option; 
  order_by: fieldname option
  (* join:  *)
}

type insert_obj = {
  table: tablename;
  fields: fieldname list option;
  values: string list
}

type delete_obj = {
  table: tablename;
  where: where_obj option
}

type create_obj = {
  table: tablename;
  fields: fieldname list;
}

(** The type [query] represents a use query that is decomposed
    into a command and possibly an object phrase. *)
type t = 
  | Select of select_obj
  | Insert of insert_obj
  | Delete of delete_obj
  | Create of create_obj
  | Quit

(** Raised when an empty query is parsed. *)
exception Empty

(** Raised when a malformed query is encountered. *)
exception Malformed of string


(* [parse str] parses a user's input into a [query], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the command. The rest of the words, if any, become the object phrase.
    Examples: 
   - [parse "    SELECT * FROM students  "] is [SELECT ["*";"FROM";"students"]]
   - [parse "QUIT"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and commas and 
    space characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the query is malformed. A query
    is {i malformed} if the command is not "QUIT","SELECT","INSERT","DELETE", 
    "JOIN", or "CREATE", or if the command is "QUIT" and there is a non-empty 
    object phrase, or if the command is any of the other option and there is an 
    empty object phrase.*)
val parse : string -> t