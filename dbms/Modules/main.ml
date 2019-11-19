open Printf
open Query
open Computation

(* [repeat_str acc str n] is the string [str] concatenated with itself
   [n] times concatednated with [acc] on the right*)
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

(*[pp_table schema output] is the printing of [schema] and [output]. The
  elements in [schema] are the  *)
let rec pp_table (fields, rows) =
  if are_rows_empty rows 
  then failwith "Row must be non empty"
  else 
    let widths = calc_widths fields rows in (* Calc width of each column *)
    let ind = ref(0) in
    let widths_lst = (Array.to_list widths) in
    pp_divider "╔═" "╦═" "╗" widths_lst;
    pp_row ind widths fields ; (* Print field names *)
    pp_divider "╠═" "╬═" "╣" widths_lst;
    pp_all_rows ind widths rows; (* Print all rows *)
    pp_divider "╚═" "╩═" "╝" widths_lst;()

(* [invalid_command ()] is the printing of ""Invalid Command, Please try 
   again.\n"" *)
let invalid_command () = ANSITerminal.(
    print_string [red] "Invalid Command, Please try again.\n")

(* [write_row row] is the concatenation of each string in [row] delimited by
   a ",". The string is ended by a newline character *)
let rec write_row = function
  | [] -> failwith "Row must be non empty"
  | h::[] -> h ^ "\n"
  | h::t -> h ^ ", " ^ write_row t

(* [concate_all_rows rows] is the concatenation of each row in [rows] using
   function write_row. Each row in [rows] is seperated by a newline character*)
let rec write_all_rows = function
  | [] -> failwith "No rows to be printed"
  | h::[] -> write_row h
  | h::t -> write_row h ^ write_all_rows t

(* [write_to_file fields rows] is the printing of fields and rows into text file
   in output folder.  Each element in each row is delimited by "," and each
   row is seperated by a newline character*)
let write_to_file fields rows=
  let oc = open_out "../output/test.txt" in   (* create file, return channel *)
  fprintf oc "%s" (write_row fields);         (* write fields *)  
  fprintf oc "%s" (write_all_rows rows);      (* write all rows *)
  close_out oc; ()                            (* flush and close the channel *)


(*[process_queries ()] is the reading, parsing, computation, and printing of 
  user queries*)
let rec process_queries () =
  print_string "> ";
  match parse (read_line ()) with 
  | exception (Empty) -> process_queries ()
  | exception (Malformed) -> invalid_command (); process_queries ()
  | command -> begin
      match command with
      | Quit -> print_endline "Goodbye for now.\n";
        exit 0
      | Select obj ->  begin
          let (fields, rows) = select obj in
          if List.length rows < 30
          then (* Print to terminal *)
            try pp_table (fields, rows); process_queries ()
            with Failure _ ->  invalid_command (); process_queries ()
          else (* Print to file *)
            write_to_file fields rows; process_queries ()
        end
      | Insert obj -> process_queries ()
      | Delete obj -> process_queries ()
      | Join obj   -> process_queries ()
      | _ -> failwith "Unimplemented"
    end 

(* [main ()] prompts the user to insert query, and then starts the engine to 
    process them *)
let main () = 
  ANSITerminal.(print_string [red] "
  \n\nWelcome to Ocaml DBMS\n");
  print_endline "Please select your database\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | db -> Datardwt.database := db;
    ANSITerminal.(print_string [blue] (
        "\n" ^ db ^ " selected. Please enter your query\n\n"));
    process_queries ()

(* Execute the dbms. *)
let () = main ()