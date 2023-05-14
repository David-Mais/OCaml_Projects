type 'a custom_llist = (unit -> 'a custom_cell) and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist);;
type 'a ocaml_llist = 'a ocaml_cell Lazy.t and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist);;
    

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

let rec is_hamming n = 
  if (n = 1) then true
  else if (n mod 2 = 0) then is_humming (n / 2)
  else if (n mod 3 = 0) then is_humming (n / 3)
  else if (n mod 5 = 0) then is_humming (n / 5)
  else false;;

let hamming_custom = 
  let rec helper n = 
    if (is_hamming n) then
      fun () -> ConsC (n, helper (n + 1))
    else 
      helper (n + 1)
    in helper 1;;

let hamming_ocaml = 
  let rec helper n = 
    if (is_hamming n) then
      lazy (ConsO (n, helper (n + 1)))
    else
      helper (n + 1)
    in helper 1;;

let test_hamming_llists () =
      let l =
        [
          __LINE_OF__ (custom_llist_to_string 14 hamming_custom =
                         "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...");
          __LINE_OF__ (custom_llist_to_string 20 hamming_custom = 
                         "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, ...");
          __LINE_OF__ (ocaml_llist_to_string 14 hamming_ocaml =
                         "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...");
          __LINE_OF__ (ocaml_llist_to_string 20 hamming_ocaml = 
                         "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, ...")
        ] in
      let result = List.fold_left (&&) true (List.map snd l) in
      if result then (Printf.printf "The test for Hamming lists succeeds.\n"; [])
      else (Printf.printf "The test for hamming lists fails.\n Check the corresponding line numbers in the list below.\n";
            (List.filter (fun (x,y) -> y=false) l) |> List.map fst)