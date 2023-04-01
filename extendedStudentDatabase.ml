type student = {
  first_name : string;
  last_name : string;
  id : int;
  semester : int;
  grade : (int * int) list;
}


type database = student list;;

let insert st db = st::db;;

let rec find_by_id id_num db = match db with
[] -> []
|x::xs -> if(x.id = id_num)
 then x::find_by_id id_num xs
else find_by_id id_num xs;;

let rec find_by_lastname lname db = match db with
[] -> []
|x::xs -> if(x.last_name = lname)
  then x::find_by_lastname lname xs
else find_by_lastname lname xs;;

let rec remove_by_id id_num db = match db with
[] -> []
|x::xs -> if(x.id <> id_num)
  then x::remove_by_id id_num xs
else remove_by_id id_num xs;;

let rec count_in_semester s db = match db with
[] -> 0
| x::xs -> (if(x.semester = s) then 1 else 0) + count_in_semester s xs;;

let list_sum l = List.fold_left (+) 0 l;;

let student_grades s = if ((List.length (List.map (fun(_, x) -> x) s.grade)) <> 0) 
  then float_of_int((list_sum (List.map (fun(_, x) -> x) s.grade))) /. float_of_int((List.length (List.map (fun(_, x) -> x) s.grade)))
  else 0.0;;

let student_avg_grade id_num db = student_grades(List.nth (find_by_id id_num db) 0);;



let rec find_course_in_list c l = match l with
[] -> []
| x::xs -> if((fst x) = c) then (snd x)::find_course_in_list c xs
else find_course_in_list c xs;;


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