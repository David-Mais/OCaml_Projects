type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha
let wc22_C = 
  [(Arg, ["Messi"], Sau, ["Al-Shehri"; "Al-Dawsari"]);
   (Mex, [], Pol, []);
   (Pol, ["Zielinski"; "Lewandowski"], Sau, []);
   (Arg, ["Messi"; "Fernandez"], Mex, []);
   (Pol, [], Arg, ["Mac Allister"; "Alvarez"]);
   (Sau, ["Al-Dawsari"], Mex, ["Martin"; "Chavez"])
  ]
let wc22_H = 
  [(Uru, [], Kor, []);
  (Por, ["Ronaldo"; "Felix"; "Leao"], Gha, ["Ayew"; "Bukari"]);
  (Kor, ["Cho Gue-sung"; "Cho Gue-sung"], Gha, ["Salisu"; "Kudus"; "Kudus"]);
  (Por, ["Fernandes"; "Fernandes"], Uru, []);
  (Kor, ["Kim Young-gwon"; "Hwang Hee-chan"], Por, ["Horta"]);
  (Gha, [], Uru, ["De Arrascaeta"; "De Arrascaeta"])
  ]

let rec match_countries list1 = match list1 with
  |[] -> []
  |(a,b,c,d)::t -> (List.map (fun players -> (players,a)) b)@(List.map (fun players -> (players,c)) d)@match_countries t;;

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
let rec include_third_element_in_touple list1 =
  match list1 with
  |[] -> []
  |((x,y),z)::t -> (x,y,z)::include_third_element_in_touple t;;

let sort_by_goal list1 = List.sort (fun (_,_,x) (_,_,y) -> compare y x) list1;;
let sort_by_name list1 = List.sort (fun (x,_,_) (y,_,_) -> compare x y) list1;;
let sort_by_both list1 = sort_by_goal (sort_by_name list1);;

let first (a,b,c,d,e,f,g,h) = a;;
let goal_difference (_,_,_,_,_,x,y,_) = x - y;;
let scorers list1 = 
  sort_by_both 
  (include_third_element_in_touple (
    count_occurrences (
      match_countries list1
    )
  ));;

let rec country_list_unfiltered list1 = 
  match list1 with
  |[] -> []
  |(a,b,c,d)::t -> a::c::country_list_unfiltered t;;
let rec country_list_filtered list1 = 
  match list1 with
  |[] -> []
  |h::t -> h::country_list_filtered (remove_element_from_list h t);;
let country_list list1 = country_list_filtered (country_list_unfiltered list1);;
let rec games_played team list1 = 
  match list1 with
  |[] -> 0
  |(a,b,c,d)::t -> if(a = team) then 1 + (games_played team t)
  else if(c = team) then 1 + (games_played team t)
  else games_played team t;;
let rec games_won team list1 = 
  match list1 with
  |[] -> 0
  |(a,b,c,d)::t -> if(team = a) then if (List.length b > List.length d) then 1 + (games_won team t) else games_won team t
  else if (team = c) then if (List.length d > List.length b) then 1 + (games_won team t) else games_won team t
  else games_won team t;;
let rec games_draw team list1 = 
  match list1 with
  |[] -> 0
  |(a,b,c,d)::t -> if(team = a) then if (List.length b = List.length d) then 1 + (games_draw team t) else games_draw team t
  else if (team = c) then if (List.length d = List.length b) then 1 + (games_draw team t) else games_draw team t
  else games_draw team t;;
let rec games_lost team list1 = 
  match list1 with
  |[] -> 0
  |(a,b,c,d)::t -> if(team = a) then if (List.length b < List.length d) then 1 + (games_lost team t) else games_lost team t
  else if (team = c) then if (List.length d < List.length b) then 1 + (games_lost team t) else games_lost team t
  else games_lost team t;;
let rec goals_for team list1 = 
  match list1 with
  |[] -> 0
  |(a,b,c,d)::t -> 
  if (team = a) then (List.length b) + goals_for team t 
  else if (team = c) then (List.length d) + goals_for team t 
  else goals_for team t;;
let rec goals_against team list1 = 
  match list1 with
  |[] -> 0
  |(a,b,c,d)::t -> if(team = a) then (List.length d) + goals_against team t
  else if(team = c) then (List.length b) + goals_against team t
  else goals_against team t;;
let rec points team list1 = 3 * (games_won team list1) + (games_draw team list1);;
let rec table_unsorted_unfiltered list1 =
  let constlist = list1 in
  match list1 with
  |[] -> []
  |(a,b,c,d)::t -> (a,(games_played a constlist),(games_won a constlist),(games_draw a constlist),(games_lost a constlist),(goals_for a constlist),(goals_against a constlist),(points a constlist))
  ::(c,(games_played c constlist),(games_won c constlist),(games_draw c constlist),(games_lost c constlist),(goals_for c constlist),(goals_against c constlist),(points c constlist))
  ::table_unsorted_unfiltered t;;
let rec remove_from_list_by_first_element element list1 = 
  match list1 with
  |[] -> []
  |h::t -> if(first h <> element) then h::remove_from_list_by_first_element element t
  else remove_from_list_by_first_element element t;;
let rec table_unsorted list1 = 
  match list1 with
  |[] -> []
  |h::t -> h::(table_unsorted (remove_from_list_by_first_element (first h) t));;
let unsortedtable list1 = table_unsorted (table_unsorted_unfiltered list1);;
let sort_by_points list1 = List.sort (fun (_,_,_,_,_,_,_,x) (_,_,_,_,_,_,_,y) -> compare y x) list1;;
let sort_by_goal_difference list1 = List.sort (fun toup1 toup2 -> compare (goal_difference toup2) (goal_difference toup1)) list1;;
let sort_by_goals_for list1 = List.sort (fun (_,_,_,_,_,x,_,_) (_,_,_,_,_,y,_,_) -> compare y x) list1;;
let random_compare x y =
  if Random.bool () then 1 else -1;;
let sort_by_random list1 =
  List.sort random_compare list1;;
let sort_by_all list1 = sort_by_points(sort_by_goal_difference(sort_by_goals_for(sort_by_random list1)));;
let table list1 = sort_by_all (unsortedtable list1);;

let table_and_scorers list1 = ((table list1),(scorers list1));;

let testing_table_and_scorers () =
  let l =
    [
      __LINE_OF__ (table_and_scorers wc22_H =
                     ([(Por, 3, 2, 0, 1, 6, 4, 6);
                       (Kor, 3, 1, 1, 1, 4, 4, 4);
                       (Uru, 3, 1, 1, 1, 2, 2, 4);
                       (Gha, 3, 1, 0, 2, 5, 7, 3)],
                      [("Cho Gue-sung", Kor, 2);
                       ("De Arrascaeta", Uru, 2);
                       ("Fernandes", Por, 2);
                       ("Kudus", Gha, 2);
                       ("Ayew", Gha, 1);
                       ("Bukari", Gha, 1);
                       ("Felix", Por, 1);
                       ("Horta", Por, 1);
                       ("Hwang Hee-chan", Kor, 1);
                       ("Kim Young-gwon", Kor, 1);
                       ("Leao", Por, 1);
                       ("Ronaldo", Por, 1);
                       ("Salisu", Gha, 1)]));
      __LINE_OF__ (table_and_scorers wc22_C =
                     ([(Arg, 3, 2, 0, 1, 5, 2, 6);
                       (Pol, 3, 1, 1, 1, 2, 2, 4);
                       (Mex, 3, 1, 1, 1, 2, 3, 4);
                       (Sau, 3, 1, 0, 2, 3, 5, 3)],
                      [("Al-Dawsari", Sau, 2);
                       ("Messi", Arg, 2);
                       ("Al-Shehri", Sau, 1);
                       ("Alvarez", Arg, 1);
                       ("Chavez", Mex, 1);
                       ("Fernandez", Arg, 1);
                       ("Lewandowski", Pol, 1);
                       ("Mac Allister", Arg, 1);
                       ("Martin", Mex, 1);
                       ("Zielinski", Pol, 1)]))
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The table_and_scorers test succeeds.\n"; [])
  else (Printf.printf "The table_and_scorers test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;