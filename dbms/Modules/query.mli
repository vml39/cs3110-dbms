(* Parsing of user queries. *)

(** The type [object_phrase] represents the object phrase that can be part of a 
    user query.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original user query.  For example:
    - If the user query is ["SCAN tableA"], then the object phrase is 
      [["tableA"]].
    - If the player query is ["SCAN     tableA"], then the object phrase is
      again [["tableA"]]. 

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list

(** The type [query] represents a use query that is decomposed
    into a verb and possibly an object phrase. *)
type t = 
  | Select of object_phrase
  | Insert of object_phrase
  | Delete of object_phrase
  | Join of object_phrase
  | Quit

(** Raised when an empty query is parsed. *)
exception Empty

(** Raised when a malformed query is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [query], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    go   clock   tower   "] is [Go ["clock"; "tower"]]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the query is malformed. A query
    is {i malformed} if the verb is neither "quit" nor "go",
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "go" and there is an empty object phrase.*)
val parse : string -> t

(* END DO NOT CHANGE
 **********************************************************************)

(* You are free to add more code here. *)