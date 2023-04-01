type student = {
  first_name : string;
  last_name : string;
  id : int;
  semester : int;
  grades : (int * float) list;
}

type database = student list;;

let insert s db = s::db;;

let rec find_by_id id_num db = match db with 
[] -> []
| x::xs -> if x.id = id_num
  then [x] 
  else find_by_id id_num xs;;

let rec find_by_lastname last db = match db with
[] -> []
| x::xs -> if x.last_name = last
  then x::find_by_lastname last xs
  else find_by_lastname last xs;;