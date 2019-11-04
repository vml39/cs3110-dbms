type t = (string list) list

type s = (string * (string list)) list

let empty = []


let parse_table_line acc_tbl acc_line filename s idx = 
  (*Remove Parens*)
  let reg = Str.regexp "//(//|//)" in
  let s' = Str.global_replace reg "" s in 
  (*Split on Commas and remove leading whitespace*)
  acc_tbl :: List.map String.trim (String.split_on_char ',' s')

let rec parse_schema_line acc_tbl acc_line filename s idx = 
  (* take table name *)
  let temp = String.split_on_char ':' in
  let temp.hd = tbl_name in
  let temp.tl = col_names in
  acc_tbl :: tbl_name * List.map String.trim (String.split_on_char ',' col_names)

let rec read_file acc_tbl filename file_channel parser =  
  try 
    let s = input_line file_channel in 
    read_file (parser acc_tbl [] filename s 0) filename  file_channel parser
  with
  | End_of_file -> close_in file_channel; acc_tbl

let schema_from_txt = 
  let file_channel = 
    open_in (Filename.parent_dir_name ^ Filename.dir_sep ^
             "input" ^ Filename.dir_sep ^ 
             "testdb" ^ Filename.dir_sep ^ 
             "schema.txt") 
  in  
  read_file empty "schema.txt" file_channel parse_schema_line


let table_from_txt filename = 
  (* Generate path to file and open it*)
  let file_channel = 
    open_in (Filename.parent_dir_name ^ Filename.dir_sep ^
             "input" ^ Filename.dir_sep ^ 
             "testdb" ^ Filename.dir_sep ^ 
             "tables" ^ Filename.dir_sep ^ filename) 
  in
  (*read it, line by line*)
  read_file empty filename file_channel parse_table_line
