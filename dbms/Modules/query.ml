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

type join_type = 
  | Inner
  | Left
  | Right
  | None

type where_obj = {
  field: fieldname;
  op: operator;
  ptn: pattern
}

type join_obj = {
  table: tablename;
  join: join_type;
  on: fieldname * fieldname
}

type select_obj = {
  table: tablename; 
  fields: fieldname list; 
  where: where_obj option; 
  order: fieldname option;
  join: join_obj option
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

type drop_obj = {
  table: tablename;
}

type help_obj = {
  s1: string;
  s2: string;
}


type t = 
  | Select of select_obj
  (* SELECT * FROM table *)
  | Insert of insert_obj
  (* INSERT INTO table_name (column1, column2, column3, ...)
     VALUES (value1, value2, value3, ...); *)
  | Delete of delete_obj
  | Create of create_obj
  | Drop of drop_obj
  | Read of string
  | Changedb of string
  | Help of help_obj
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
let create_where_record fieldname h op = 
  {
    field = new_field fieldname h;
    op = op;
    ptn = "";
  }

(** TODO: document *)
let rec select_where_ptn ptn where_rec (record: select_obj) = function 
  | [] -> 
    raise (Malformed "Must provide a pattern to match with after WHERE operator")
  | h::i::j::t when i = "ORDER" && j = "BY" -> 
    order_by "" {
      record with 
      where = Some {where_rec with ptn = new_field ptn h}
    } t
  | h::t when t = [] -> {
      record with 
      where = Some {where_rec with ptn = new_field ptn h}
    }
  | h::t -> select_where_ptn (new_field ptn h) where_rec record t

(** TODO: document *)
let rec select_where fieldname where_rec (record : select_obj) = function
  | [] ->
    raise (Malformed "Must provide a field, operator and pattern after 'WHERE'")
  | h::op::t when op = "=" -> 
    select_where_ptn "" (create_where_record fieldname h EQ) record t
  | h::op::t when op = ">" ->
    select_where_ptn "" (create_where_record fieldname h GT) record t
  | h::op::t when op = "<" ->
    select_where_ptn "" (create_where_record fieldname h LT) record t
  | h::op::t when op = ">=" ->
    select_where_ptn "" (create_where_record fieldname h GEQ) record t
  | h::op::t when op = "<=" ->
    select_where_ptn "" (create_where_record fieldname h LEQ) record t
  | h::op::t when op = "<>" ->
    select_where_ptn "" (create_where_record fieldname h NEQ) record t
  | h::op::t when op = "LIKE" ->
    select_where_ptn "" (create_where_record fieldname h Like) record t
  | h::t -> select_where (new_field fieldname h) where_rec record t

(** TODO: document *)
let select_join_qry (record: select_obj) = function
  | [] -> record
  | h::t when h = "WHERE" -> 
    let init_where = {
      field = "";
      op = None;
      ptn = ""
    } in 
    select_where "" init_where record t
  | h::i::t when h = "ORDER" && i = "BY" ->
    order_by "" record t
  | _ -> 
    raise (Malformed "'JOIN' can only be followed by 'WHERE' or 'ORDER BY'")

(** TODO: document *)
let select_join_field (join_rec: join_obj) (record: select_obj) = function 
  | [] -> 
    raise (Malformed "You must provide a column from each table to join on")
  | h::i::j::t when i = "=" -> 
    let new_join = {
      join_rec with 
      on = (h, j)
    } in 
    select_join_qry {record with join = Some new_join} t
  | _ -> 
    raise (Malformed "You must provide a column from each table to join on")

(** TODO: document *)
let select_join (join_rec: join_obj) (record: select_obj) = function 
  | [] -> raise (Malformed "You must join 'ON' another table")
  | h::i::t when i = "ON" -> 
    select_join_field {join_rec with table = h} record t
  | _ -> raise (Malformed "You must join 'ON' another table")

(** TODO: document *)
let match_join join = 
  if join = "INNER" then Inner 
  else if join = "LEFT" then Left
  else if join = "RIGHT" then Right
  else raise (Malformed "Invalid join operation")

(** TODO: document *)
let select_table (record : select_obj) = function
  | [] ->  raise (Malformed "No table specified")
  | h::[] -> {record with table = h}
  | h::i::j::t when j = "JOIN" -> 
    let init_join = {
      table = "";
      join = match_join i;
      on = ("", "")
    } in 
    let new_record = {
      record with 
      table = h;
      join = Some init_join
    } in 
    select_join init_join new_record t
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

(** [select_fields acc fieldname record q] is [record] with 
    fields = [fieldname] *)
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

(** [select_parse lst] is a select_obj if [lst] follows the select query rules.
    Malformed otherwise.*)
let select_parse = function
  | [] -> raise 
            (Malformed "You must include the field names and tables to be selected")
  | lst -> 
    let init_rec = {
      table = ""; 
      fields = []; 
      where = None; 
      order = None;
      join = None
    } in
    select_fields [] "" init_rec lst

(** [insert_fields acc value record lst] is [record] with 
    values = [value] filled in if [lst] follows the insert query rules. 
    Malformed otherwise *)
let rec insert_values acc value (record: insert_obj) = function 
  | [] -> raise (Malformed "You must specify a valid list of values")
  | h::i::t when i = ")" && t = [] -> 
    {record with values = (List.rev ((new_field value h)::acc))}
  | h::i::t when i = "," -> 
    insert_values ((new_field value h)::acc) "" record t
  | h::t -> insert_values acc (new_field value h) record t

(** [insert_fields acc fieldname record lst] is an [record] with 
    fields = [fieldname], and values filled in if [lst] follows the insert query 
    rules.  Malformed otherwise *)
let rec insert_fields acc fieldname (record: insert_obj) = function 
  | [] -> raise (Malformed "You must specify values")
  | h::i::j::k::t when i = ")" && j = "VALUES" && k = "(" -> 
    let new_record = {record with 
                      fields = Some (List.rev ((new_field fieldname h)::acc))} in
    insert_values [] "" new_record t
  | h::i::t when i = "," -> 
    insert_fields ((new_field fieldname h)::acc) "" record t
  | h::t -> insert_fields acc (new_field fieldname h) record t

(** [insert_parse t] is an insert_obj with table, fields, and values filled in
    if [t] follows the insert query rules. Malformed otherwise *)
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

(** [delete_where_ptn ptn where_rec record lst] is a delete_obj if [lst] follows
    the delete where query rules *)
let rec delete_where_ptn ptn where_rec (record : delete_obj) = function 
  | [] -> {record with where = Some {where_rec with ptn = ptn}}
  | h::t -> delete_where_ptn (new_field ptn h) where_rec record t

(** [delete_where fieldname where_rec record lst] is a delete_obj if 
    if lst matches one of the valid where operators*)
let rec delete_where fieldname where_rec (record : delete_obj) = function
  | [] -> {record with where = Some where_rec}
  | h::op::t when op = "=" -> 
    delete_where_ptn "" (create_where_record fieldname h EQ) record t
  | h::op::t when op = ">" ->
    delete_where_ptn "" (create_where_record fieldname h GT) record t
  | h::op::t when op = "<" ->
    delete_where_ptn "" (create_where_record fieldname h LT) record t
  | h::op::t when op = ">=" ->
    delete_where_ptn "" (create_where_record fieldname h GEQ) record t
  | h::op::t when op = "<=" ->
    delete_where_ptn "" (create_where_record fieldname h LEQ) record t
  | h::op::t when op = "<>" ->
    delete_where_ptn "" (create_where_record fieldname h NEQ) record t
  | h::op::t when op = "LIKE" ->
    delete_where_ptn "" (create_where_record fieldname h Like) record t
  | h::t -> delete_where (new_field fieldname h) where_rec record t

(** [delete_parse lst] is a delete_obj if lst follows the delete query rules.
    Malformed otherwise. *)
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
  | _ -> raise (Malformed 
                  "You can delete all values from a table or specify a WHERE condition") 

(** [create_fields acc fieldname lst] is the list of fields to be populated by
    the create query *)
let rec create_fields acc fieldname = function
  | [] -> raise (Malformed "Invalid field names")
  | h::i::t when i = ")" && t = [] -> List.rev ((new_field fieldname h)::acc)
  | h::i::t when i = "," -> create_fields ((new_field fieldname h)::acc) "" t
  | h::t -> create_fields acc (new_field fieldname h) t 

(** [create_table_parse lst] is a create_obj if [lst] matches the create table
    query rules.  Malformed otherwise*)
let create_table_parse = function 
  | [] -> raise (Malformed "Must specify fields for the new table")
  | h::i::t when i = "(" -> {
      table = h;
      fields = create_fields [] "" t
    }
  | _ -> raise (Malformed "Invalid CREATE TABLE query")


(** [drop_table_parse lst] is a drop_obj if [lst] matches the drop table 
    query rules. Malformed otherwise.*)
let drop_table_parse = function 
  | [] -> raise (Malformed "Must specify table to delete")
  | h::[] -> {
      table = h;
    }
  | _ -> raise (Malformed "Invalid DROP TABLE query")


(** [help_parse lst] is a help_obj if [lst] follows the help query rules *)
let help_parse = function 
  | [] -> {
      s1 = "";
      s2 = "";
    }
  | s1::s2::[] -> {
      s1 = String.uppercase_ascii s1;
      s2 = String.uppercase_ascii s2;
    }
  | h::[] -> {
      s1 = String.uppercase_ascii h;
      s2 = "";
    }
  | _ -> raise (Malformed "please specify a command from the approved list")

(* [parse str] is the parsing of [str].  [str] is split into a list with each 
   word, ',', '(', and ')' as a seperate element.  The list is then parsed 
   according to the type of command it is identified as.  Illegal queries
   result in a <Malformed message>.*)
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
  | h::i::t when h = "TRUNCATE" && i = "TABLE" -> Delete (delete_parse t)
  | h::i::t when h = "CREATE" && i = "TABLE" -> Create (create_table_parse t)
  | h::i::t when h = "DROP" && i = "TABLE" -> Drop (drop_table_parse t)
  | h::i::t::[] when h = "READ" && i = "FROM" -> Read t
  | h::i::t::[] when h = "CHANGE" && i = "DATABASE" -> Changedb t
  | h::t when h = "HELP" -> Help (help_parse t)
  | h::t when h = "help" -> raise (Malformed "Did you mean 'HELP'?")
  | h::t when h = "QUIT" || h = "quit" || h = "q" -> 
    if t <> [] 
    then raise (Malformed "If you would like to quit, please type QUIT") 
    else Quit
  | _ -> raise (Malformed "Illegal query")