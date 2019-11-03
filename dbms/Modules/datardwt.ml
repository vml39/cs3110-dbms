type t = (string list) list

type s = unit

let empty = []

let schema_from_txt = 
  failwith "unimplimented"

let rec parse_line acc_tbl acc_line filename s idx = 
  let r' = Str.regexp ("pls no") in
  match Str.search_forward r' s idx with 
  | exception Not_found -> acc_line :: acc_tbl
  (* print_endline ("word is " ^ word); *)
  | i -> let word = Str.matched_string s in 
    parse_line acc_tbl (word :: acc_line) filename s (i+String.length word)

let rec read_file acc_tbl filename file_channel =  
  try 
    let s = input_line file_channel in 
    read_file (parse_line acc_tbl [] filename s 0) filename  file_channel
  with
  | End_of_file -> close_in file_channel; acc_tbl

let table_from_txt filename = 
  (* Generate path to file and open it*)
  let file_channel = 
    open_in (Filename.parent_dir_name ^ Filename.dir_sep ^
             "input" ^ Filename.dir_sep ^ 
             "testdb" ^ Filename.dir_sep ^ 
             "tables" ^ Filename.dir_sep ^ filename) in
  (*read it, line by line*)
  read_file empty filename file_channel