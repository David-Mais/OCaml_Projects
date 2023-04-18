(*
  todo Write a function member, which takes a comparision function c, a term t and a 
  todo list l and returns true if l contains an element e such that e and t are equal 
  todo with respect to c
*)
let rec member c t l = match l with
|[] -> false
|head::tail -> if(c t head) then true
else member c t tail;;

(*todo write a compare function that returns true if two numbers are equal else it returns false*)
(* let compare a b =
  if(a = b) then true
  else false;; *)


  (* TODO write a function that uses the previously defined "compare" function *)
  (* TODO to compare second elements of touples *)

let equal_second_components (_, x) (_, y) = compare x y;;

  (* TODO write a function that makes odd integers equal to each other and return true*)
  (* TODO and even integers equal to each other and return true*)
let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2);;


(* ! This is the test you should run in order to test the correctness of the program*)
(* ? run it by typing in utop "testing_number ();;" if it returns "The member test succeeds."
   ? you did everything right*)
let testing_member () =
  let l =
    [
      __LINE_OF__ ((member compare 3 [1; 2; 3]) = true);
      __LINE_OF__ ((member compare 4 [1; 2; 3]) = false);
      __LINE_OF__ ((member compare 'a' ['a'; 'b'; 'c']) = true);
      __LINE_OF__ ((member equal_second_components ('a',5) [(1,2); (3,4); (5,6)]) = false);
      __LINE_OF__ ((member equal_second_components ('a',6) [(1,2); (3,4); (5,6)]) = true);
      __LINE_OF__ ((member equal_second_components (42, 6) [(1,2); (3,4); (5,6)]) = true);
      __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 2; 3]) = true);
      __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 3; 5]) = false);
   ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The member test succeeds.\n"; [])
  else (Printf.printf "The member test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;