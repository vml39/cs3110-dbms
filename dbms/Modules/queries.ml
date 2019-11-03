(* type entry = 
   | String of string 
   | Int of int 
   | Bool of bool

   type table = 
   | Nil 
   | Cons of entry * entry list *)

type object_phrase = string list

type query = 
  | Select of object_phrase
  (* SELECT * FROM table *)
  | Insert of object_phrase
  (* INSERT INTO table_name (column1, column2, column3, ...)
     VALUES (value1, value2, value3, ...); *)
  | Delete of object_phrase
  | Join of object_phrase
  | Quit

exception Empty

exception Malformed

(** [join_string_list acc sl] is the string concatenation of [sl] with a space
    in between [sl] entries. *)
let rec join_string_list acc = function
  | [] -> acc
  | h::t -> join_string_list (acc^" "^h) t

(* [get_string_list str] is the string list of words in [str] deliminated by
   spaces and commas.  Example: "SELECT cats,dogs FROM tableA, tableB" is 
   ["Select","cats","dogs","FROM","tableA","tableB"] *)
let get_string_list str =
  let remove_space str = 
    (str |> String.split_on_char ' ' |> List.filter ( fun s -> s <> "")) in
  let rec remove_comma = function
    | [] -> []
    | h::t when String.contains h ',' -> (String.split_on_char 'h' h)@(remove_comma t)
    | h::t -> h::(remove_comma t) in
  str |> remove_space |> remove_comma

let parse str =
  match get_string_list str with
  | [] -> raise Empty
  | h::t when h = "SELECT" -> if t = [] then raise Malformed else Select t
  | h::t when h = "INSERT" -> if t = [] then raise Malformed else Insert t
  | h::t when h = "DELETE" -> if t = [] then raise Malformed else Delete t
  | h::t when h = "JOIN" -> if t = [] then raise Malformed else Join t
  | h::t when h = "QUIT" -> if t <> [] then raise Malformed else Quit
  | _ -> raise Malformed