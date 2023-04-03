(* TODO 
   1) Define a data type for a student. For every student the first name, last name, 
   identification number, current semester as well as the grades received in different 
   courses have to be stored. A course is simply represented by a number.
  
   2) Define database as a list of students.
   3) Write a function insert : student -> database -> database that inserts a student into the database.
   4) Write a function find_by_id : int -> db -> student list that returns a list with all 
   students with the given id (either a single student or an empty list, if no such student exists).
   5) Implement a function find_by_last_name : string -> database -> student list to find all students with a given last name.

*)
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