type l = string list

type t = string list list

type s = (string * (string list)) list

let empty = []

(*[parse_schema_line a s] is the line from the schema s parsed into a
  tuple fo string and string list for further computation*)
let rec parse_schema_line acc_tbl s = 
  (* take table name *)
  let temp = String.split_on_char ':' s in
  match temp with 
  | h :: s :: [] ->
    (h , (List.map String.trim (String.split_on_char ',' s))) :: acc_tbl
  | _ -> failwith "Invalid schema pattern"

(*[read_file a f fc] is the list of lines from file f*)
let rec read_schema acc_tbl filename file_channel =  
  try 
    let s = input_line file_channel in 
    read_schema (parse_schema_line acc_tbl s) filename  file_channel
  with
  | End_of_file -> close_in file_channel; List.rev acc_tbl

let schema_from_txt () = 
  let file_channel = 
    open_in (Filename.parent_dir_name ^ Filename.dir_sep ^
             "input" ^ Filename.dir_sep ^ 
             "testdb" ^ Filename.dir_sep ^ 
             "schema.txt") 
  in  
  let throw_out = input_line file_channel in
  read_schema empty "schema.txt" file_channel

(*[parse_table_line a s] is the line from the table s parsed into a
  string list for further computation*)
let parse_table_line acc_tbl s = 
  (*Remove Parens*)
  let reg = Str.regexp "(\\|)" in
  let s' = Str.global_replace reg "" s in 
  (*Split on Commas and remove leading whitespace*)
  (List.map String.trim (String.split_on_char ',' s')) :: acc_tbl


(*[read_file a f fc] is the list of lines from file f*)
let rec read_file acc_tbl filename file_channel =  
  try 
    let s = input_line file_channel in 
    read_file (parse_table_line acc_tbl s) filename file_channel
  with
  | End_of_file -> close_in file_channel; List.rev acc_tbl

(*[table_from_txt f] is a list of string list where each string list is 
  a row in the table named f*)
let table_from_txt filename = 
  (* Generate path to file and open it*)
  let file_channel = 
    open_in (Filename.parent_dir_name ^ Filename.dir_sep ^
             "input" ^ Filename.dir_sep ^ 
             "testdb" ^ Filename.dir_sep ^ 
             "tables" ^ Filename.dir_sep ^ filename ^ ".txt") 
  in
  (*read it, line by line*)
  read_file empty filename file_channel 


let get_file_chan filename =
  open_in (Filename.parent_dir_name ^ Filename.dir_sep ^
           "input" ^ Filename.dir_sep ^ 
           "testdb" ^ Filename.dir_sep ^ 
           "tables" ^ Filename.dir_sep ^ filename ^ ".txt") 

let next_line fc =
  let s = input_line fc in
  let reg = Str.regexp "(\\|)" in
  let s' = Str.global_replace reg "" s in 
  (*Split on Commas and remove leading whitespace*)
  (List.map String.trim (String.split_on_char ',' s'))

