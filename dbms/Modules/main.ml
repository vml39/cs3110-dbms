open Datardwt
open Query
open Computation

<<<<<<< HEAD
let get_lengths schema output = 
  let schema_lens = Array.of_list (List.map (fun s -> String.length s) schema) in 
  let output_lens = Array.of_list (List.map (fun lst -> Array.of_list (List.map (fun s -> String.length s) lst)) output) in
=======
let rec process_queries () =
  print_string "> ";
  (*Read the command from Terminal *)
  let comm = read_line () in
  (*Try to Parse it *)
  match parse comm with 
  | exception (Empty) -> process_queries ()
  | exception (Malformed) -> 
    ANSITerminal.(print_string [red] 
                    "Invalid Command, Please try again.\n");
    process_queries ()
  | msg -> begin
      match msg with
      | Quit -> print_endline "Goodbye for now.\n";
        exit 0
      | Select obj -> select obj
      | _ -> failwith "unimplemented"
    end

let get_length schema output = 
  let schema_lengths = List.map (fun s -> String.length s) schema in 
  let output_lengths = List.fold_left (fun acc lst -> (List.fold_left (fun acc s -> String.length s + acc) 0 lst)::acc) [] output in
>>>>>>> 11f2b6a1a46ddcb0abcb1109fed46e6c55f1269f
  failwith "unimplemented"

let rec repeat_string str length = 
  if length = 0 then "" else str ^ repeat_string str (length - 1)

let pp_word element =
  print_string " ";
  print_string element;
  print_string (repeat_string " " (19 - String.length element));()

let rec pp_divider num_columns = 
  match num_columns with
  | 0 -> ()
  | 1 -> print_string ((repeat_string "-" 20) ^ "\n");
  | n -> print_string ((repeat_string "-" 20) ^ "+"); pp_divider (n-1)

(*[pp_table schema output] is the printing of [schema] and [output]. The
  elements in [schema] are the  *)
let rec pp_table (schema,output) =
  let rec pp_row  = function
    | [] -> print_endline ""
    | h::[] -> pp_word h; print_endline ""
    | h::t -> pp_word h; print_string "|"; pp_row t in
  let rec pp_output = function
    | [] -> ()
    | h::t ->  pp_row h; pp_output t in
  pp_row schema;
  pp_divider (List.length schema);
  pp_output output; ()


let rec process_queries () =
  print_string "> ";
  (*Read the command from Terminal *)
  let command = read_line () in
  (*Try to Parse it *)
  match parse command with 
  | exception (Empty) -> process_queries ()
  | exception (Malformed) -> 
    ANSITerminal.(print_string [red] 
                    "Invalid Command, Please try again.\n");
    process_queries ()
  | msg -> begin
      match msg with
      | Quit -> print_endline "Goodbye for now.\n";
        exit 0
      | Select obj ->  pp_table (Computation.select obj); process_queries ()
    end

let main () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Ocaml DBMS\n");
  print_endline "Please enter your query\n";
  process_queries ()

let () = main ()