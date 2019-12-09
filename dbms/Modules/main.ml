open Printf
open Query
open Computation

(* [repeat_str acc str n] is the string [str] concatenated with itself
   [n] times concatenated with [acc] on the right*)
let rec repeat_str acc str n = 
  if n = 0 
  then acc
  else repeat_str (str ^ acc) str (n - 1)

(*[pp_divider left inter right widths] is the printing of (length [width]) 
  "═" segments. The first "═" segment is preceded by [left] and the last
  segment is followed by [right] and new line. Between each segment of "═" there 
  is an [inter]. The length of each segment of "═" is determined by the
  corresponding int in [widths] i.e. the first segment's length is the int in 
  the first element of [width], the second segment length is the second int in 
  [width] and so on*)
let rec pp_divider left inter right widths =
  print_string left;
  let rec pp_divider_rec = function
    | [] -> ()
    | h::[] -> print_string (repeat_str "" "═" (h));
    | h::t -> print_string ((repeat_str "" "═" (h)) ^ inter); pp_divider_rec t in 
  pp_divider_rec widths;
  print_endline right; ()

(* [pp_word width word] is the printing of [word] with one space on the left and
   ([width] - length [word]) spaces on the right *)
let pp_word width word =
  print_string "║ ";
  print_string word;
  print_string (repeat_str "" " " (width - String.length word))

(*[pp_row ind widths row] is the printing of each element in [row] using the 
  function pp_word. At the end of this function call a newline is printed and
   [ind] is reset to 0 
   Required: [widths] and [row] must have same dimension greater than zero *)
let rec pp_row ind widths = function
  | [] -> failwith "Row must be non empty"
  | h::[] -> 
    pp_word widths.(!ind) h ; 
    ind := 0; 
    print_endline "║"
  | h::t ->  
    pp_word widths.(!ind) h; 
    ind := !ind + 1; 
    pp_row ind widths t 

(*[pp_all_rows ind widths rows] is the printing of each list in [rows] using
  the function pp_row. *)
let rec pp_all_rows ind widths = function
  | [] -> ()
  | h::t -> pp_row ind widths h ; pp_all_rows ind widths t 

(*[calc_widths fields rows] is the int array containing the max string lengths
  of each dimension from [fields] and every list in [rows].
  Required: [fields] and every list in [rows] must have same dimension *)
let calc_widths fields rows = 
  let max_widths = Array.of_list (List.map String.length fields) in 
  let rows = Array.of_list 
      List.(map (fun lst -> (Array.of_list (map String.length lst))) rows) in
  for i = 0 to ((Array.length rows) - 1) do 
    for j = 0 to ((Array.length max_widths) - 1) do
      max_widths.(j) <- max max_widths.(j) rows.(i).(j)
    done
  done; 
  max_widths |> Array.map (fun s -> s + 1)

(*[are_rows_empty rows] is whether the lists inside list [row] are empty or if
  list [row] is empty *)
let are_rows_empty = function
  | [] -> true
  | []::t -> true
  | _ -> false

(* [pp_table (fields, rows)] is the printing of [fields] and [rows] as a table
   with [fields] at the top and [rows] below seperated by unicode characters*)
let rec pp_table (fields, rows) =
  let widths = calc_widths fields rows in (* Calc width of each column *)
  let ind = ref(0) in
  let widths_lst = (Array.to_list widths) in
  print_endline ("Number of records: " ^ (string_of_int (List.length rows)));
  pp_divider "╔═" "╦═" "╗" widths_lst;
  pp_row ind widths fields ; (* Print field names *)
  pp_divider "╠═" "╬═" "╣" widths_lst;
  pp_all_rows ind widths rows; (* Print all rows *)
  pp_divider "╚═" "╩═" "╝" widths_lst;()

(* [invalid_command ()] is the printing of ""Invalid Command, Please try 
   again.\n"" *)
let invalid_command () = ANSITerminal.(
    print_string [red] "Invalid Command, Please try again.\n")

(* [Malformed exception s] prints the exception [s] *)
let malformed_exception s = ANSITerminal.(
    print_string [red] ("Malformed Query: " ^ s ^ "\n"))

(* [sys exception s] prints the file system error [s]*)
let sys_exception s = ANSITerminal.(
    print_string [red] ("File System Exception: " ^ s ^ "\n"))

(* [invalid_db] prints the error message [s] when an invalid database is input
   in the system *)
let invalid_db s = ANSITerminal.(
    print_string [red] ("Database " ^ s ^ " does not exist, please try again\n"))

(* [invalid_file s] prints the error message [s] when an invalid file is input
   into the system *)
let invalid_file s = ANSITerminal.(
    print_string [red] ("File " ^ s ^ " does not exist, please try again\n"))

(* [write_row row] is the concatenation of each string in [row] delimited by
   a ",". The string is ended by a newline character *)
let rec write_row = function
  | [] -> failwith "Row must be non empty"
  | h::[] -> h ^ "\n"
  | h::t -> h ^ ", " ^ write_row t

(* [concate_all_rows rows] is the concatenation of each row in [rows] using
   function write_row. Each row in [rows] is seperated by a newline character*)
let rec write_all_rows = function
  | [] -> ""
  | h::[] -> write_row h
  | h::t -> write_row h ^ write_all_rows t

(* [write_to_file fields rows query_from_file filename query oc_option] is the 
   printing of fields and rows into text file in output folder. Each element in 
   each row is delimited by "," & each row is seperated by a newline character*)
let write_to_file fields rows num query_from_file filename query oc_option=
  match oc_option with
  | Some oc ->
    fprintf oc "%s" (query ^ "\n");
    fprintf oc "%s" (write_row fields);         (* write fields *)  
    fprintf oc "%s" ((write_all_rows rows) ^ "\n");      (* write all rows *)
    ()
  | None ->
    let oc = open_out (".." ^ Filename.dir_sep ^ "output" ^ Filename.dir_sep 
                       ^ "query" ^ (string_of_int !num) ^ ".txt") in  
    num := !num +1;
    fprintf oc "%s" (write_row fields);         (* write fields *)  
    fprintf oc "%s" (write_all_rows rows);      (* write all rows *)
    close_out oc; 
    ANSITerminal.(print_string [green] (
        "\n" ^ "Query written to " ^ ".." ^ Filename.dir_sep ^ "output" 
        ^ Filename.dir_sep ^ "query" ^ (string_of_int !num) ^ ".txt\n\n"));()

(* [compute_commands_from_file command query filename oc] is the computation of
   [command] and writing the result to [oc]*)
let compute_commands_from_file command query filename oc =
  begin 
    try
      match command with
      | Quit -> print_endline "Goodbye for now.\n";
        exit 0
      | Select obj -> 
        let (fields, rows) = select obj in
        write_to_file fields rows (ref (-1)) true filename query (Some oc)
      | Insert obj -> insert obj; fprintf oc "%s" (query ^ "\n\n")
      | Delete obj -> delete obj; fprintf oc "%s" (query ^ "\n\n") 
      | Create obj -> create_table obj; fprintf oc "%s" (query ^ "\n\n")
      | Drop obj ->  drop_table obj; fprintf oc "%s" (query ^ "\n\n")
      | Read _ -> failwith "You cannot use READ FROM in a file"  
      | Help obj -> failwith "You cannot ask for HELP in a file" 
      | Changedb s -> failwith "You cannot use CHANGE DATABASE in a file"
    with
    | Query.Malformed s -> malformed_exception s
    | Sys_error s -> sys_exception s
  end 

(** [rd_wt_queries_from_file inc oc filename] is the reading and writing of the
    computed queries in [filename]*)
let rec rd_wt_queries_from_file inc oc filename =
  try
    let query = input_line inc in
    match parse query  with
    | exception (Empty) -> (); rd_wt_queries_from_file inc oc filename
    | exception (Query.Malformed "") -> 
      invalid_command (); rd_wt_queries_from_file inc oc filename
    | exception (Query.Malformed s) -> 
      malformed_exception s; rd_wt_queries_from_file inc oc filename
    | command -> compute_commands_from_file command query filename oc;
      rd_wt_queries_from_file inc oc filename
  with  End_of_file -> 
    ANSITerminal.(print_string [green] (
        "\n" ^ "Queries written to " ^ ".." ^ Filename.dir_sep ^ "output" 
        ^ Filename.dir_sep  ^ filename ^ "_processed" ^ ".txt\n\n"));
    close_in inc


(* [queries_from_file filename] is the computation of the queries in [filename] 
*)
let queries_from_file filename = 
  let filepath = (".." ^ Filename.dir_sep ^ "input" ^ Filename.dir_sep 
                  ^ "commands" ^ Filename.dir_sep ^ filename ^ ".txt") in
  if Sys.file_exists filepath
  then 
    let inc = open_in filepath in
    let oc = open_out_gen [Open_append; Open_creat] 0o666
        (".." ^ Filename.dir_sep ^ "output" ^ Filename.dir_sep 
         ^ filename ^ "_processed" ^ ".txt") in 
    rd_wt_queries_from_file inc oc filename;
    close_out oc
  else invalid_file filename

(* [change_db] is the switch from the current database to the new database [db].
   If the database doesn't exist, an error message is displayed*)
let change_db db = 
  if (not (Sys.file_exists (Filename.parent_dir_name ^ Filename.dir_sep ^
                            "input" ^ Filename.dir_sep ^ db)))
  then invalid_db db
  else begin 
    Datardwt.database := db;
    ANSITerminal.(print_string [green] (
        "\n" ^ db ^ " selected. Please enter your query\n\n"))
  end

(* [table_height rows] is the # of lines [rows] would take up as a table with
   an additional row for fields, 1 row for dividers, and 2 rows for boundaries*)
let table_height rows = 4 + List.length rows 

(* [table_width rows] is the # of characters the longest list in [rows] 
   would take up as a table *)
let table_width fields rows = 
  Array.fold_left (fun a i -> a + i) 0 (calc_widths fields rows) +
  (List.length fields) * 2

(* [print_or_write_select obj num] is the display of select commands in either
     terminal if the terminal screen is big enough to display the entire table or 
     to a file otherwise*)
let print_or_write_select obj num = 
  begin
    let terminal_w, terminal_h = ANSITerminal.size () in 
    let (fields, rows) = select obj in
    let table_width =  table_width fields rows  in
    let table_height = table_height rows in
    if table_height < terminal_h && table_width < terminal_w
    then (* Print to terminal *)
      try pp_table (fields, rows)
      with Failure s ->  malformed_exception s
    else (* Print to file *)
      write_to_file fields rows num false "" "" None
  end

(* [compute_command_from_terminal command num] is the computation of commands.
   READ, CHANGE DATABASE, and HELP commands are allowed*)
let compute_command_from_terminal command num = 
  try
    match command with
    | Quit -> print_endline "Goodbye for now.\n";
      exit 0
    | Select obj -> print_or_write_select obj num
    | Insert obj -> insert obj
    | Delete obj -> delete obj
    | Create obj -> create_table obj
    | Drop obj ->  drop_table obj
    | Read file -> queries_from_file file 
    | Changedb db' -> change_db db'
    | Help obj -> print_string (help_with obj) 
  with
  | Query.Malformed s -> malformed_exception s
  | Sys_error s -> sys_exception s

(*[process_queries ()] is the reading, parsing, computation, and printing of 
  user queries*)
let rec process_queries num () =
  print_string "> ";
  match parse (read_line ()) with 
  | exception (Empty) -> process_queries num ()
  | exception (Query.Malformed "") -> invalid_command (); process_queries num () 
  | exception (Query.Malformed s) -> 
    malformed_exception s; process_queries num ()
  | command -> compute_command_from_terminal command num; process_queries num ()

(* [read_input_db ()] checks if inputed database exists and continues to prompt
   for a database until the user uses the QUIT command, which exits the program, 
   or inputs a valid database, at which point the user is prompted for a query
   and engine to process queries is called *)
let rec read_input_db () =
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | exception (Empty) -> read_input_db ()
  | "" -> read_input_db ()
  | "QUIT" -> print_endline "Goodbye for now.\n"; exit 0
  | "quit" -> print_endline "Goodbye for now.\n"; exit 0
  | "q" -> print_endline "Goodbye for now.\n"; exit 0
  | db -> begin
      if (not (Sys.file_exists (Filename.parent_dir_name ^ Filename.dir_sep ^
                                "input" ^ Filename.dir_sep ^ db)))
      then begin invalid_db db;   
        read_input_db () end
      else
        Datardwt.database := db;
      ANSITerminal.(print_string [green] (
          "\n" ^ db ^ " selected. Please enter your query\n\n"));
      process_queries (ref 1) ()
    end

(* [main ()] prompts the user to select a database, and then calls read_input_db
   to check if database exists*)
let main () = 
  ANSITerminal.(print_string [red] "
   \n\nWelcome to Ocaml DBMS\nType 'HELP' for help with Ocaml DBMS commands\n");
  print_endline "Please select your database\n";
  read_input_db ()

(* Execute the dbms. *)
let () = main ()