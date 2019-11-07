open Query
open Computation

(* [repeat_str acc str width] is the string [str] concatenated with itself
   [width] times concatednated with [acc] on the right*)
let rec repeat_str acc str width = 
  if width = 0 
  then acc
  else repeat_str (str ^ acc) str (width - 1)

(*[pp_divider widths] is the printing of (length [width]) hyphen segments. Each
  segment of hyphen is determined by the value in each element of [width]. 
  Between each two segments of hyphens there is a "+" to separate. The last 
  segment is followed by a new line.*)
let rec pp_divider = function
  | [] -> ()
  | h::[] -> print_string ((repeat_str "" "-" (h+1)) ^ "\n");
  | h::t -> print_string ((repeat_str "" "-" (h+1)) ^ "+"); pp_divider t

(* [pp_word width word] is the printing of [word] with one space on the left and
   ([width] - length [word]) spaces on the right *)
let pp_word width word =
  print_string " ";
  print_string word;
  print_string (repeat_str "" " " (width - String.length word))

(*[pp_row ind widths row] is the printing of each element in [row] using the 
  function pp_word. At the end of this function call a newline is printed and
   [ind] is reset to 0 
   Required: [widths] and [row] must have same dimension greater than zero *)
let rec pp_row ind widths = function
  | [] -> failwith "Cannot print empty row"
  | h::[] -> 
    pp_word widths.(!ind) h ; 
    ind := 0; 
    print_string "\n"
  | h::t ->  
    pp_word widths.(!ind) h; 
    print_string "|"; 
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

(*[pp_table schema output] is the printing of [schema] and [output]. The
  elements in [schema] are the  *)
let rec pp_table (fields, rows) =
  let widths = calc_widths fields rows in (* Calc width of each column *)
  let ind = ref(0) in
  pp_row ind widths fields ;
  pp_divider (Array.to_list widths);
  pp_all_rows ind widths rows; ()

(*[process_queries ()] is the reading, parsing, computation, and printing of 
  user queries*)
let rec process_queries () =
  print_string "> ";
  match parse (read_line ()) with 
  | exception (Empty) -> process_queries ()
  | exception (Malformed) -> 
    ANSITerminal.(print_string [red] "Invalid Command, Please try again.\n");
    process_queries ()
  | msg -> begin
      match msg with
      | Quit -> print_endline "Goodbye for now.\n";
        exit 0
      | Select obj ->  begin
          try pp_table (select obj); process_queries ()
          with _ -> ANSITerminal.(
              print_string [red] "Invalid Command, Please try again.\n");
            process_queries () end 
      | Insert obj -> process_queries ()
      | Delete obj -> process_queries ()
      | Join obj -> process_queries ()
    end

(* [main ()] prompts the user to insert query, and then starts the engine to 
    process them *)
let main () = 
  ANSITerminal.(print_string [red] "\n\nWelcome to Ocaml DBMS\n");
  print_endline "Please enter your query\n";
  process_queries ()

(* Execute the dbms. *)
let () = main ()