type 'a custom_llist = (unit -> 'a custom_cell) and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist)
type 'a ocaml_llist = 'a ocaml_cell Lazy.t and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist)

let rec from_to_custom from to_ step =
  if from <= to_
  then fun () -> ConsC (from, from_to_custom (from + step) to_ step)
  else fun () -> NilC;;

let rec print_custom_llist n c_list =
if n != 0
then match c_list () with
   | NilC -> print_string "Nil\n"
   | ConsC (h, t) ->
      Printf.printf "%d, " h;
      print_custom_llist (n-1) t
else print_string "...\n";;

let rec custom_llist_to_string n c_list =
if n != 0
then match c_list () with
| NilC -> "Nil"
| ConsC (h, t) ->
   string_of_int h ^ ", " ^
     custom_llist_to_string (n-1) t
else "...";;

let rec from_to_ocaml from to_ step =
  if from <= to_
  then lazy (ConsO (from, from_to_ocaml (from + step) to_ step))
  else lazy NilO;;

let rec print_ocaml_llist n o_list =
if n != 0
then match Lazy.force o_list with
| NilO -> print_string "Nil\n"
| ConsO (h, t) ->
   Printf.printf "%d, " h;
   print_ocaml_llist (n-1) t
else print_string "...\n";;

let rec ocaml_llist_to_string n o_list =
if n != 0
then match Lazy.force o_list with
| NilO -> "Nil"
| ConsO (h, t) ->
   string_of_int h ^ ", " ^
     ocaml_llist_to_string (n-1) t
else "...";;


let rec merge_custom_llists l1 l2 = match l1 (), l2 () with
  |NilC, _ -> l2
  |_, NilC -> l1
  |ConsC (a,b), ConsC (c, d) -> if(a < c) then fun () -> ConsC (a, merge_custom_llists b l2)
  else fun () -> ConsC (c, merge_custom_llists l1 d);;

let rec merge_ocaml_llists l1 l2 = 
  match Lazy.force l1, Lazy.force l2 with
  |NilO, _ -> l2
  |_, NilO -> l1
  |ConsO (a,b), ConsO(c,d) -> if(a < c) then lazy (ConsO (a, merge_ocaml_llists b l2))
  else lazy (ConsO (c, merge_ocaml_llists l1 d));;



let test_merge_llists () =
    let l =
      [
        __LINE_OF__ (custom_llist_to_string 13
          (merge_custom_llists (from_to_custom 0 5 1) (from_to_custom 0 5 1)) =
                       "0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, Nil");
        __LINE_OF__ (custom_llist_to_string 13
                       (merge_custom_llists (from_to_custom 0 5 1) (from_to_custom 6 5 1)) =
                       "0, 1, 2, 3, 4, 5, Nil");
        __LINE_OF__ (ocaml_llist_to_string 13
          (merge_ocaml_llists (from_to_ocaml 0 5 1) (from_to_ocaml 0 5 1)) =
                       "0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, Nil");
        __LINE_OF__ (ocaml_llist_to_string 13
                       (merge_ocaml_llists (from_to_ocaml 0 5 1) (from_to_ocaml 6 5 1)) =
                       "0, 1, 2, 3, 4, 5, Nil")
      ] in
    let result = List.fold_left (&&) true (List.map snd l) in
    if result then (Printf.printf "The test for merging over lazy lists succeeds.\n"; [])
    else (Printf.printf "The test for merging over lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
          (List.filter (fun (x,y) -> y=false) l) |> List.map fst)