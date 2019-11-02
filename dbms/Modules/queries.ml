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

(** [trim_list acc sl] is [sl] with all whitespace and empty strings 
    removed and returned in [acc]. *)
let rec trim_list acc = function
  | [] -> acc
  | h::t ->
    if String.trim h = "" then trim_list acc t
    else trim_list (h::acc) t

let parse str =
  let trimmed_str = String.trim str |> String.split_on_char ' ' in
  match trimmed_str with
    | [] -> raise Empty
    | [""] -> raise Empty
    | h::t -> 
      if h = "quit" && t = [] then Quit 
      else if h = "go" && t != [] then Go (List.rev (trim_list [] t))
      else if h = "score" && t = [] then Score
      else if h = "take" && t != []
        then Take (String.trim (join_string_list "" t))
      else if h = "drop" && t != []
        then Drop (String.trim (join_string_list "" t))
      else if h = "inventory" && t = [] then Inventory
      else raise Malformed