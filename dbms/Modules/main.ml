open Datardwt
open Query
open Computation

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

let calc_lengths fields output = 
  let column_lens = Array.of_list (List.map (fun s -> String.length s) fields) in 
  let output_lens = Array.of_list (List.map (fun lst -> (Array.of_list (List.map (fun s -> String.length s) lst))) output) in
  for i = 0 to ((Array.length output_lens) - 1) do 
    for j = 0 to ((Array.length column_lens) - 1) do
      column_lens.(j) <- max column_lens.(j) output_lens.(i).(j)
    done
  done; 
  column_lens |> Array.map (fun s -> s + 1)

let rec repeat_string str length = 
  if length = 0 then "" else str ^ repeat_string str (length - 1)

let pp_word element length=
  print_string " ";
  print_string element;
  print_string (repeat_string " " (length - String.length element));()

let rec pp_divider = function
  | [] -> ()
  | h::[] -> print_string ((repeat_string "-" (h+1)) ^ "\n");
  | h::t -> print_string ((repeat_string "-" (h+1)) ^ "+"); pp_divider t
(* match num_columns with
   | 0 -> ()
   | 1 -> print_string ((repeat_string "-" 20) ^ "\n");
   | n -> print_string ((repeat_string "-" 20) ^ "+"); pp_divider (n-1) *)

(*[pp_table schema output] is the printing of [schema] and [output]. The
  elements in [schema] are the  *)
let rec pp_table (fields, output) =
  let col_lens = calc_lengths fields output in
  let rec pp_row ind row = 
    (ind := !ind + 1);
    match row with
    | [] -> print_endline ""
    | h::[] -> pp_word h col_lens.(!ind); (ind := -1); print_endline ""
    | h::t ->  pp_word h col_lens.(!ind); print_string "|"; pp_row ind t in
  let rec pp_output output =
    let ind = ref(-1) in
    match output with
    | [] -> ()
    | h::t -> pp_row ind h; pp_output t in
  let ind = ref(-1) in
  pp_row ind fields;
  pp_divider (Array.to_list col_lens);
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
      | Insert obj -> process_queries ()
      | Delete obj -> process_queries ()
      | Join obj -> process_queries ()
    end

let main () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Ocaml DBMS\n");
  print_endline "Please enter your query\n";
  process_queries ()

let () = main ()