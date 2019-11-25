type object_phrase = string list

type t = 
  | Select of object_phrase
  (* SELECT * FROM table *)
  | Insert of object_phrase
  (* INSERT INTO table_name (column1, column2, column3, ...)
     VALUES (value1, value2, value3, ...); *)
  | Delete of object_phrase
  | Join of object_phrase
  | Create of object_phrase
  | Quit

type fieldname = string

type tablename = string

type pattern = string

type operator = | EQ | GT | LT | GEQ | LEQ | Like | NEQ | None

type where_obj = {field: fieldname;
                  op: operator;
                  ptn: pattern}

type select_obj = {table: tablename; 
                   fields: fieldname list; 
                   where: where_obj option; 
                   order_by:  fieldname option}

type delete = {table: tablename;
               where: where_obj option}

type insert = {table: tablename;
               fields: fieldname list;
               values: string list}

exception Empty

exception Malformed of string

let new_field s1 s2 =
  if s1 = "" 
  then s2
  else s1 ^ " " ^ s2

let rec order_by fieldname (record : select_obj) = function
  | [] -> {record with order_by = Some fieldname}
  | h::t -> order_by (new_field fieldname h) record t

let create_where_record fieldname h op ptn = 
  {
    field = new_field fieldname h;
    op = op;
    ptn = ptn;
  }

let rec select_where fieldname where_rec (record : select_obj) = function
  | [] -> {record with where = Some where_rec}
  | h::i::t when h = "ORDER" && i = "BY" -> 
    order_by "" {record with where = Some where_rec} t
  | h::op::ptn::t when op = "=" -> 
    select_where "" (create_where_record fieldname h EQ ptn) record t
  | h::op::ptn::t when op = ">" ->
    select_where "" (create_where_record fieldname h GT ptn) record t
  | h::op::ptn::t when op = "<" ->
    select_where "" (create_where_record fieldname h LT ptn) record t
  | h::op::ptn::t when op = ">=" ->
    select_where "" (create_where_record fieldname h GEQ ptn) record t
  | h::op::ptn::t when op = "<=" ->
    select_where "" (create_where_record fieldname h LEQ ptn) record t
  | h::op::ptn::t when op = "<>" ->
    select_where "" (create_where_record fieldname h NEQ ptn) record t
  | h::op::ptn::t when op = "LIKE" ->
    select_where "" (create_where_record fieldname h Like ptn) record t
  | h::t -> select_where (new_field fieldname h) where_rec record t

let select_table (record : select_obj) = function
  | [] ->  raise (Malformed "No table specified")
  | h::[] -> {record with table = h}
  | h::i::t when i = "WHERE" -> 
    let new_record = {record with table = h} in
    let init_where = { field = ""; op = None; ptn = ""} in
    select_where "" init_where new_record t
  | h::i::j::t when i = "ORDER" && j = "BY" && t <> [] -> 
    let new_record = {record with table = h} in 
    order_by "" new_record t
  | _ -> raise (Malformed "Scoopity oops woops")

let rec select_fields acc fieldname (record : select_obj) = function
  | [] ->  raise (Malformed "No 'FROM' keyword")
  | h::i::t when i = "FROM" -> 
    let new_record = {record with fields = List.rev (fieldname::acc)} in 
    select_table new_record t
  | h::i::t when i = "," -> select_fields (h::acc) "" record t
  | h::t -> select_fields acc (new_field fieldname h) record t


let select_parse = function
  | [] -> raise (Malformed "You must include the field names and tables to be selected")
  | lst -> let init_rec = {table = ""; fields = []; where = None; order_by = None} in
    select_fields [] "" init_rec lst

let parse str =
  match str 
        |> Str.global_replace (Str.regexp "[ ]+") " " 
        |> Str.global_replace (Str.regexp ",") " , " 
        |> Str.global_replace (Str.regexp "(") " ( " 
        |> Str.global_replace (Str.regexp "(") " ( " 
        |> String.split_on_char ' ' 
        |> List.filter ( fun s -> s <> "") with
  | [] -> raise Empty
  | h::t when h = "SELECT" -> select_parse t
  | h::t when h = "INSERT" -> if t = [] then raise Malformed else Insert t
  | h::t when h = "DELETE" -> if t = [] then raise Malformed else Delete t
  | h::t when h = "JOIN" -> failwith "unimplemented"
  | h::h'::t when h = "CREATE" && h' = "TABLE" -> 
    if t = [] then raise Malformed else Create t
  | h::t when h = "QUIT" -> if t <> [] then raise Malformed else Quit
  | _ -> raise Malformed

let parse str =
  match str 
        |> Str.global_replace (Str.regexp "[\( \) ,]") " " 
        |> Str.replace_first (Str.regexp ";.+") ""
        |> String.split_on_char ' ' 
        |> List.filter ( fun s -> s <> "") with
  | [] -> raise Empty
  | h::t when h = "SELECT" -> if t = [] then raise Malformed else Select t
  | h::t when h = "INSERT" -> if t = [] then raise Malformed else Insert t
  | h::t when h = "DELETE" -> if t = [] then raise Malformed else Delete t
  | h::t when h = "JOIN" -> if t = [] then raise Malformed else Join t
  | h::h'::t when h = "CREATE" && h' = "TABLE" -> 
    if t = [] then raise Malformed else Create t
  | h::t when h = "QUIT" -> if t <> [] then raise Malformed else Quit
  | _ -> raise Malformed