let rec drop_last l = 
  match l with 
  |[] -> raise (Failure "Empty list has no last element")
  |[x] -> []
  |h::t -> h::drop_last t;;


  let testing_drop_last () =
    let l =
      [
        __LINE_OF__ ((drop_last [1; 2; 3; 4]) = [1; 2; 3]);
        __LINE_OF__ ((drop_last [1]) = []);
        __LINE_OF__ ((try Some (drop_last []) with (Failure _) -> None) = None) (* If this line is reported during testing, you have an rrror in raising Failure *)
      ] in
    let result = List.fold_left (&&) true (List.map snd l) in
    if result then (Printf.printf "The drop_last test succeeds.\n"; [])
    else (Printf.printf "The drop_last test fails.\n Check the corresponding line numbers in the list below.\n";
          (List.filter (fun (x,y) -> y=false) l) |> List.map fst);;