exception Empty

exception Malformed of string

type object_phrase = string list

type fieldname = string

type tablename = string

type pattern = string

type operator = 
  | EQ 
  | GT 
  | LT 
  | GEQ 
  | LEQ 
  | Like 
  | NEQ 
  | None

type where_obj = {
  field: fieldname;
  op: operator;
  ptn: pattern
}

type select_obj = {
  table: tablename; 
  fields: fieldname list; 
  where: where_obj option; 
  order: fieldname option
  (* join:  *)
}

type delete_obj = {
  table: tablename;
  where: where_obj option
}

type insert_obj = {
  table: tablename;
  fields: fieldname list option;
  values: string list
}

type create_obj = {
  table: tablename;
  fields: fieldname list;
}

type t = 
  | Select of select_obj
  (* SELECT * FROM table *)
  | Insert of insert_obj
  (* INSERT INTO table_name (column1, column2, column3, ...)
     VALUES (value1, value2, value3, ...); *)
  | Delete of delete_obj
  | Create of create_obj
  | Quit

(** TODO: document *)
let new_field s1 s2 =
  if s1 = "" 
  then s2
  else s1 ^ " " ^ s2

(** TODO: document *)
let rec order_by fieldname (record : select_obj) = function
  | [] when fieldname = "" -> 
    raise (Malformed "Must provide a field to order by")
  | [] -> {record with order = Some fieldname}
  | h::t -> order_by (new_field fieldname h) record t

(** TODO: document *)
let create_where_record fieldname h op ptn = 
  {
    field = new_field fieldname h;
    op = op;
    ptn = ptn;
  }

(** TODO: document *)
let rec select_where fieldname where_rec (record : select_obj) = function
  | [] when where_rec.field = "" || where_rec.op = None || where_rec.ptn = "" ->
    raise (Malformed "Must provide a field, operator and pattern after 'WHERE'")
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

(** TODO: document *)
let select_table (record : select_obj) = function
  | [] ->  raise (Malformed "No table specified")
  | h::[] -> {record with table = h}
  | h::i::t when i = "WHERE" -> 
    let new_record = {record with table = h} in
    let init_where = {
      field = "";
      op = None;
      ptn = ""
    } in
    select_where "" init_where new_record t
  | h::i::j::t when i = "ORDER" && j = "BY" -> 
    let new_record = {record with table = h} in 
    order_by "" new_record t
  | _ -> raise 
    (Malformed "The table name can only be followed by 'WHERE' or 'ORDER BY'")

(** TODO: document *)
let rec select_fields acc fieldname (record : select_obj) q = 
  match q with 
  | [] -> raise (Malformed "Field names malformed or no 'FROM' keyword")
  | h::i::t when h <> "," && i = "FROM" -> 
    let new_record = {record with 
                      fields = List.rev ((new_field fieldname h)::acc)} in 
    select_table new_record t
  | h::i::t when i = "," -> 
    select_fields ((new_field fieldname h)::acc) "" record t
  | h::t -> select_fields acc (new_field fieldname h) record t

(** TODO: document *)
let select_parse = function
  | [] -> raise 
    (Malformed "You must include the field names and tables to be selected")
  | lst -> 
    let init_rec = {
      table = ""; 
      fields = []; 
      where = None; 
      order = None
    } in
    select_fields [] "" init_rec lst

let rec print_list = function 
  | [] -> ()
  | h::t -> print_string (h ^ " ")

(** TODO: document *)
let rec insert_values acc value (record: insert_obj) = function 
  | [] -> raise (Malformed "You must specify a valid list of values")
  | h::i::t when i = ")" && t = [] -> 
    let values = (List.rev ((new_field value h)::acc)) in 
    print_list values;
    {record with values = (List.rev ((new_field value h)::acc))}
  | h::i::t when i = "," -> 
    insert_values ((new_field value h)::acc) "" record t
  | h::t -> insert_values acc (new_field value h) record t

(** TODO: document *)
let rec insert_fields acc fieldname (record: insert_obj) = function 
  | [] -> raise (Malformed "You must specify values")
  | h::i::j::k::t when i = ")" && j = "VALUES" && k = "(" -> 
    let new_record = {record with 
                      fields = Some (List.rev ((new_field fieldname h)::acc))} in
    insert_values [] "" new_record t
  | h::i::t when i = "," -> 
    insert_fields ((new_field fieldname h)::acc) "" record t
  | h::t -> insert_fields acc (new_field fieldname h) record t

(** TODO: document *)
let insert_parse t =
  let init_record = {
    table = "";
    fields = None;
    values = []
  } in 
  match t with   
  | [] -> raise (Malformed "You must specify a table name")
  | h::i::j::t when i = "VALUES" && j = "(" -> insert_values [] "" 
    {init_record with table = h} t
  | h::i::t when i = "(" -> 
    insert_fields [] "" {init_record with table = h} t
  | _ -> raise (Malformed "Table name must be followed by a list of fields")

(** TODO: document *)
let rec delete_where fieldname where_rec (record : delete_obj) = function
  | [] -> {record with where = Some where_rec}
  | h::op::ptn::t when op = "=" -> 
    delete_where "" (create_where_record fieldname h EQ ptn) record t
  | h::op::ptn::t when op = ">" ->
    delete_where "" (create_where_record fieldname h GT ptn) record t
  | h::op::ptn::t when op = "<" ->
    delete_where "" (create_where_record fieldname h LT ptn) record t
  | h::op::ptn::t when op = ">=" ->
    delete_where "" (create_where_record fieldname h GEQ ptn) record t
  | h::op::ptn::t when op = "<=" ->
    delete_where "" (create_where_record fieldname h LEQ ptn) record t
  | h::op::ptn::t when op = "<>" ->
    delete_where "" (create_where_record fieldname h NEQ ptn) record t
  | h::op::ptn::t when op = "LIKE" ->
    delete_where "" (create_where_record fieldname h Like ptn) record t
  | h::t -> delete_where (new_field fieldname h) where_rec record t

(** TODO: document *)
let delete_parse = function 
  | [] -> raise (Malformed "You must specify a table name")
  | h::t when t = [] -> {
      table = h;
      where = None
    }
  | h::i::t when i = "WHERE" -> 
    let new_record = {
      table = h;
      where = None
    } in
    let init_where = {
      field = "";
      op = None;
      ptn = ""
    } in
    delete_where "" init_where new_record t
  | _ -> raise (Malformed "deletttt") 

(** TODO: document *)
let rec create_fields acc fieldname = function
  | [] -> raise (Malformed "Invalid CREATE TABLE query")
  | h::i::t when i = ")" && t = [] -> List.rev ((new_field fieldname h)::acc)
  | h::i::t when i = "," -> create_fields ((new_field fieldname h)::acc) "" t
  | h::t -> create_fields acc (new_field fieldname h) t 

(** TODO: document *)
let create_table_parse = function 
  | [] -> raise (Malformed "Must specify fields for the new table")
  | h::i::t when i = "(" -> {
      table = h;
      fields = create_fields [] "" t
    }
  | _ -> raise (Malformed "Invalid CREATE TABLE query")

let parse str =
  match str 
        |> Str.global_replace (Str.regexp "[ ]+") " " 
        |> Str.global_replace (Str.regexp ",") " , " 
        |> Str.global_replace (Str.regexp "(") " ( " 
        |> Str.global_replace (Str.regexp ")") " ) " 
        |> String.split_on_char ' ' 
        |> List.filter ( fun s -> s <> "") with
  | [] -> raise Empty
  | h::t when h = "SELECT" -> Select (select_parse t)
  | h::i::t when h = "INSERT" && i = "INTO" -> Insert (insert_parse t)
  | h::i::t when h = "DELETE" && i = "FROM" -> Delete (delete_parse t)
  | h::i::t when h = "CREATE" && i = "TABLE" -> Create (create_table_parse t)
  | h::t when h = "QUIT" -> 
    if t <> [] 
    then raise (Malformed "If you would like to quit, please type QUIT") 
    else Quit
  | _ -> raise (Malformed "Illegal query")