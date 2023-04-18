let rec count_number_of_elements e l = 
  match l with
  |[] -> 0
  |h::t -> if(h = e) then 1 + count_number_of_elements e t
  else count_number_of_elements e t;;

let rec remove_element_from_list e l = 
  match l with 
  |[] -> []
  |h::t -> if (h <> e) then h::remove_element_from_list e t
  else remove_element_from_list e t;;

let rec count_occurrences_unsorted listt = 
  match listt with
  |[] -> []
  |h::t -> (h, (count_number_of_elements h listt))::count_occurrences_unsorted (remove_element_from_list h listt);;
let sort_touples listt = List.sort (fun (_,x) (_,y) -> compare y x) listt;;
let count_occurrences list1= sort_touples (count_occurrences_unsorted list1);;

let testing_count_occurrences () =
  let l =
    [
      __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
      __LINE_OF__ ((count_occurrences ['a'; 'b'; 'a'; 'c'; 'c'; 'a'; 'd']) = [('a', 3); ('c', 2); ('b', 1); ('d', 1)]);
      __LINE_OF__ ((count_occurrences [0; 0; 0; -2; 3; -1; -1; 3; 3; 0]) = [(0, 4); (3, 3); (-1, 2); (-2, 1)]);
      __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The count_occurrences test succeeds.\n"; [])
  else (Printf.printf "The count_occurrences test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;