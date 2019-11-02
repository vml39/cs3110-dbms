(* type entry = 
   | String of string 
   | Int of int 
   | Bool of bool

   type table = 
   | Nil 
   | Cons of entry * entry list *)

type object_phrase = string list

type command = 
  | Scan of object_phrase
  | Select of object_phrase
  (* SELECT * FROM table *)
  | Insert of object_phrase
  (* INSERT INTO table_name (column1, column2, column3, ...)
     VALUES (value1, value2, value3, ...); *)
  | Delete of object_phrase
  | Where of object_phrase
  | Like of object_phrase
  | Order of object_phrase
  | Sort of object_phrase
  | Join of object_phrase
  | Quit

exception Empty

exception Malformed

(** [join_string_list acc sl] is the string concatenation of [sl] with a space
    in between [sl] entries. *)
let rec join_string_list acc = function
  | [] -> acc
  | h::t -> join_string_list (acc^" "^h) t

let parse str =
  match (str |> String.split_on_char ' ' |> List.filter ( fun s -> s <> "")) with
  | [] -> raise Empty
  | h::t when h = "SCAN" -> if t = [] then raise Malformed else Scan t
  | h::t when h = "SELECT" -> if t = [] then raise Malformed else Select t
  | h::t when h = "INSERT" -> if t = [] then raise Malformed else Insert t
  | h::t when h = "DELETE" -> if t = [] then raise Malformed else Delete t
  | h::t when h = "WHERE" -> if t <> [] then raise Malformed else Where t
  | h::t when h = "LIKE" -> if t <> [] then raise Malformed else Like t
  | h::t when h = "ORDER" -> if t <> [] then raise Malformed else Order t
  | h::t when h = "SORT" -> if t <> [] then raise Malformed else Sort t
  | h::t when h = "JOIN" -> if t <> [] then raise Malformed else Join t
  | h::t when h = "quit" -> if t <> [] then raise Malformed else Quit
  | _ -> raise Malformed