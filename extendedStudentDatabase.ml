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

(*HERE STARTS THE FIRST PART*)
type student = {
  first_name : string;
  last_name : string;
  id : int;
  semester : int;
  grade : (int * int) list;
}
(*HERE ENDS THE FIRST PART*)


(*HERE STARTS THE SECOND PART*)
type database = student list;;
(*HERE ENDS THE SECOND PART*)


(*HERE STARTS THE THIRD PART*)
let insert st db = st::db;;
(*HERE ENDS THE THIRD PART*)


(*HERE STARTS THE FORTH PART*)
let rec find_by_id id_num db = match db with
[] -> []
|x::xs -> if(x.id = id_num)
 then x::find_by_id id_num xs
else find_by_id id_num xs;;
(*HERE ENDS THE FORTH PART*)


(*HERE STARTS THE FIFTH PART*)
let rec find_by_lastname lname db = match db with
[] -> []
|x::xs -> if(x.last_name = lname)
  then x::find_by_lastname lname xs
else find_by_lastname lname xs;;
(*HERE ENDS THE FIFTH PART*)


(*
TODO remove_by_id : int -> database -> database removes the student 
TODO with the given id from the database.*)
(*
  * NOW THIS IS INTERESTING SINCE LISTS ARE IMMUTABLE IN
  * OCAML WE HAVE TO ADD EVERYTHING INTO NEW LIST EXCEPT THE 
  * ONE WITH THE GIVEN ID   
*)
let rec remove_by_id id_num db = match db with
[] -> []
|x::xs -> if(x.id <> id_num)
  then x::remove_by_id id_num xs
else remove_by_id id_num xs;;


(*
  TODO count_in_semester : int -> database -> int 
  TODO counts the number of students in the given semester. [1 Point]*)

(*
  *I just wrote simpe recursive function to count the number of students
  *who are in the given semester  
*)
let rec count_in_semester s db = match db with
[] -> 0
| x::xs -> (if(x.semester = s) then 1 else 0) + count_in_semester s xs;;

(*
  TODO student_avg_grade : int -> database -> float computes the average 
  TODO grade of the student with the given id. If no student with the given 
  TODO id exists or the student does not have any grades, the function shall return 0.0.
*)
(** The function list_sum does simple thing it sums up and returns all the ints in the list   *)
let list_sum l = List.fold_left (+) 0 l;;

(**the fucntion student_grades returns the average score of the given list*)
let student_grades s = if ((List.length (List.map (fun(_, x) -> x) s.grade)) <> 0) 
  then float_of_int((list_sum (List.map (fun(_, x) -> x) s.grade))) /. float_of_int((List.length (List.map (fun(_, x) -> x) s.grade)))
  else 0.0;;

  (* 
     * and finally this function uses previous two functions to find the final answer
  *)
let student_avg_grade id_num db = student_grades(List.nth (find_by_id id_num db) 0);;


(*
  TODO course_avg_grade : int -> database -> float computes the average grade 
  TODO achieved in the given course. If no grades in the given course exist,
  TODO the function shall return 0.0   
*)
(*
 * this function take in as parametest course as int and database and searches and returns
 * the list containing all the touples that have the corresponding integer in the first value
*)
let rec find_course_in_list c l = match l with
[] -> []
| x::xs -> if((fst x) = c) then (snd x)::find_course_in_list c xs
else find_course_in_list c xs;;

(* 
  *now this fucntion takes those touples we have extracte in prevoius function and puts their
  *second values in another list so that we have every grade in one list
*)
let rec course_list_creator db = match db with
[] -> []
|x::xs -> x.grade @ course_list_creator xs;;

let course_avg_grade c db = if(List.length(find_course_in_list c (course_list_creator db)) <> 0)
  then float_of_int(list_sum(find_course_in_list c (course_list_creator db))) /. float_of_int(List.length(find_course_in_list c (course_list_creator db)))
else 0.0;;


let db = [];;
let a = {first_name="a"; last_name="a"; id=1; semester=1; grade=[(1, 2); (2, 4); (3, 10)]};;
let b = {first_name="b"; last_name="b"; id=2; semester=1; grade=[(1, 2)]};;
let c = {first_name="c"; last_name="c"; id=3; semester=3; grade=[(1, 30)]};;
let d = {first_name="d"; last_name="d"; id=4; semester=1; grade=[]};;
let e = {first_name="e"; last_name="e"; id=5; semester=5; grade=[(1, 2)]};;