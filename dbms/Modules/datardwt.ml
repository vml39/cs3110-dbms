type l = string list

type t = string list list

type s = (string * (string list)) list

let database = ref ""

let empty = []

let set_database () = read_line ()

(*[parse_schema_line a s] is the line from the schema s parsed into a
  tuple fo string and string list for further computation*)
let rec parse_schema_line acc_tbl s = 
  (* take table name *)
  let temp = String.split_on_char ':' s in
  match temp with 
  | h :: s :: [] ->
    (h , (List.map String.trim (String.split_on_char ',' s))) :: acc_tbl
  | _ -> failwith "Invalid schema pattern"

(*[read_schema a f fc] is the list of lines from schema f*)
let rec read_schema acc_tbl filename file_channel =  
  try 
    let s = input_line file_channel in 
    read_schema (parse_schema_line acc_tbl s) filename  file_channel
  with
  | End_of_file -> close_in file_channel; List.rev acc_tbl

(*[schema_from_text] is the full schema of the current database*)
let schema_from_txt () = 
  let file_channel = 
    open_in (Filename.parent_dir_name ^ Filename.dir_sep ^
             "input" ^ Filename.dir_sep ^ 
             !database ^ Filename.dir_sep ^ 
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

(*[table_from_txt f] is a list of string lists where each string list is 
  a row in the table named f*)
let table_from_txt filename = 
  (* Generate path to file and open it*)
  let file_channel = 
    open_in (Filename.parent_dir_name ^ Filename.dir_sep ^
             "input" ^ Filename.dir_sep ^ 
             !database ^ Filename.dir_sep ^ 
             "tables" ^ Filename.dir_sep ^ filename ^ ".txt") 
  in
  (*read it, line by line*)
  read_file empty filename file_channel 

(* [get_path f] is the string  representation of the file path to f*)
let get_path filename =
  (Filename.parent_dir_name ^ Filename.dir_sep ^
   "input" ^ Filename.dir_sep ^ 
   !database ^ Filename.dir_sep ^ 
   "tables" ^ Filename.dir_sep ^ filename ^ ".txt") 

(* [get_in_chan f] is the input channel to the file f*)
let get_in_chan filename =
  open_in (get_path filename) 

(* [get_out_chan f] is the output channel to the file f, if f does not exist,
   it is created*)
let get_out_chan filename =
  open_out_gen [Open_append; Open_creat] 0o666
    (get_path filename) 

(* [get_out_chan f] is the output channel to the file f, if f does not exist,
   it is created*)
let get_schema_path () =  
  Filename.parent_dir_name ^ Filename.dir_sep ^
  "input" ^ Filename.dir_sep ^ 
  !database ^ Filename.dir_sep ^ "schema.txt"

(* [get_schema_temp_path] is the string representation of the path to the temp
   schema*)
let get_schema_temp_path () =  
  Filename.parent_dir_name ^ Filename.dir_sep ^
  "input" ^ Filename.dir_sep ^ 
  !database ^ Filename.dir_sep ^ "schema.tmp"

(* [get_out_chan_schema] is the output channel to the schema*)
let get_out_chan_schema () = 
  open_out_gen [Open_append] 0o666
    (get_schema_path ())

(* [get_in_chan_schema] is the input channel to the schema*)
let get_in_chan_schema () = 
  open_in (get_schema_path ())

let get_out_chan_temp_schema () = 
  open_out_gen [Open_append; Open_creat] 0o666
    (get_schema_temp_path ())

let read_next_line inc =
  let s = input_line inc in
  let reg = Str.regexp "(\\|)" in
  let s' = Str.global_replace reg "" s in 
  (*Split on Commas and remove leading whitespace*)
  (List.map String.trim (String.split_on_char ',' s'))


let read_next_schema_line inc =
  let s = input_line inc in
  let name_and_fields  = String.split_on_char ':' s in
  let name = List.hd name_and_fields in
  let feilds = List.hd (List.tl name_and_fields) in
  (*Split on Commas and remove leading whitespace*)
  let field_list = (List.map String.trim (String.split_on_char ',' feilds)) in
  (name, field_list)

let write_line outc lst = 
  let single_a = 
    "(" ^ (List.fold_left (fun acc s -> acc ^ s ^ ", " ) "" lst) in
  let single_b = String.sub single_a 0 (String.length single_a - 2) ^ ")\n" in
  output_string outc single_b

let write_line_table_schema outc table lst = 
  let single_a = 
    table ^ ": " ^ (List.fold_left (fun acc s -> acc ^ s ^ ", " ) "" lst) 
  in
  let single_b = String.sub single_a 0 (String.length single_a - 2) ^ "\n" in
  output_string outc single_b

let write_line_schema outc lst = 
  let single_a = 
    (List.fold_left (fun acc s -> acc ^ s ^ ", " ) "" lst) 
  in
  let single_b = String.sub single_a 0 (String.length single_a - 2) ^ "\n" in
  output_string outc single_b

let delete_line fc lst = failwith "unimplemented"

