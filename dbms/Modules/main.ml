open Datardwt
open Queries
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
      | Select obj -> ()
    end


let main () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Ocaml DBMS\n");
  print_endline "Please enter your query\n";
  process_queries ()